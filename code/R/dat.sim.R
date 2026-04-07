#' Simulate Task-Specific Coefficients Around a Baseline
#'
#' @description
#' Creates a task coefficient matrix by perturbing a baseline \code{B0} with
#' Gaussian noise scaled by \code{h} and shifted by \code{eta}.
#'
#' @param B0 Baseline coefficient matrix of size \code{v x mi}.
#' @param h Numeric scale for task variability.
#' @param eta Numeric mean shift for the perturbation.
#' @param n Integer. Number of observations per task (not used directly by this generator but
#'   included for symmetry with data simulation).
#' @param v Integer. Number of predictors.
#' @param mi Integer. Number of incomplete outcomes (or targets) driven by \code{B0}.
#'
#' @return A \code{v x mi} numeric matrix of task-specific coefficients.
#'
#' @examples
#' B <- simulate_task(B0 = matrix(0, 10, 3), h = 0.5, eta = 0, n = 100, v = 10, mi = 3)
#' dim(B)
#' @export
#' @importFrom stats rnorm
#' @importFrom MASS mvrnorm
#' 
library(stats)
library(MASS) 

simulate_task <- function(B0, h, eta, n, v, mi) {
  B <- B0 + h * matrix(rnorm(v * mi, mean = eta, sd = 1), nrow = v, ncol = mi)
  return(B)
}

#' Generate Synthetic Multi-Task Datasets
#'
#' @description
#' Produces a single synthetic task with user-controlled proportion of complete vs. incomplete
#' variables and outcome types (Gaussian/Bernoulli). Used by higher-level task batch simulators.
#'
#' @param n Integer. Sample size (rows).
#' @param p Integer. Total number of variables.
#' @param s Integer. Number of variables considered complete (fully observed).
#' @param v Integer. Number of predictors used in the regression layer.
#' @param B Coefficient matrix controlling incomplete targets (typically from \code{simulate_task()}).
#' @param complete_data_type Named numeric vector giving mixture weights for complete variables,
#'   e.g., \code{c(norm = 1, binom = 0)}.
#' @param incomplete_data_type Named numeric vector giving mixture weights for incomplete variables.
#'
#' @return A list with elements \code{data} (data.frame), \code{B} (coefficients), and
#'   \code{covariates} (indices or metadata on predictors).
#'
#' @examples
#' set.seed(1)
#' B <- simulate_task(matrix(0, 10, 2), h=0.3, eta=0, n=100, v=10, mi=2)
#' sim <- generate_data(n=100, p=20, s=10, v=10, B=B,
#'                      complete_data_type=c(norm=1, binom=0),
#'                      incomplete_data_type=c(norm=1, binom=0))
#' str(sim$data)
#' @importFrom stats rnorm rbinom runif
#' @import MASS
#' @export

generate_data <- function(n = 100, p = 40, s = 35, v = 10, B, 
                          complete_data_type = c(norm = 1, binom = 0),
                          incomplete_data_type = c(norm = 1, binom = 0)) {
  
  complete_data_type <- complete_data_type / sum(complete_data_type)
  incomplete_data_type <- incomplete_data_type / sum(incomplete_data_type)
  
  num_norm_complete <- round(s * complete_data_type["norm"])
  num_binom_complete <- round(s * complete_data_type["binom"])
  
  num_norm_incomplete <- round((p - s) * incomplete_data_type["norm"])
  num_binom_incomplete <- round((p - s) * incomplete_data_type["binom"])
  ind_incomplete <- c(rep('n', num_norm_incomplete), rep('b', num_binom_incomplete))
  
  Sigma <- outer(1:num_norm_complete, 1:num_norm_complete, function(x,y){
    0.5^(abs(x-y))
  })
  eps <- rnorm(num_norm_complete, sd = 0.3)
  Sigma <- Sigma + eps %*% t(eps)
  R <- chol(Sigma)
  
  complete_norm <- matrix(rnorm(n * num_norm_complete), nrow = n) %*% R
  complete_binom <- matrix(rbinom(n * num_binom_complete, 1, 0.5), nrow = n)
  
  complete_data <- cbind(complete_norm, complete_binom)
  colnames(complete_data) <- paste0("c_x", 1:ncol(complete_data))
  
  incomplete_norm <- matrix(0, n, num_norm_incomplete)
  incomplete_binom <- matrix(0, n, num_binom_incomplete)
  covariates_norm <- matrix(NA, v, num_norm_incomplete)
  covariates_binom <- matrix(NA, v, num_binom_incomplete)
  
  total_subsets <- floor(s / v)
  usable_cols <- total_subsets * v
  subset_indices <- split(seq_len(usable_cols), rep(1:total_subsets, each = v))
  
  assign_subsets <- function(num_vars, subsets) {
    total_subsets <- length(subsets)
    
    if (num_vars <= total_subsets) {
      selected_subsets <- sample(subsets, num_vars, replace = FALSE)
    } else {
      # If we must reuse subsets, use all subsets first, then sample again
      full_cycles <- num_vars %/% total_subsets
      remainder <- num_vars %% total_subsets
      selected_subsets <- rep(subsets, full_cycles)
      if (remainder > 0) {
        selected_subsets <- c(selected_subsets, sample(subsets, remainder, replace = FALSE))
      }
    }
    return(selected_subsets)
  }
  
  norm_subsets <- assign_subsets(num_norm_incomplete, subset_indices)
  binom_subsets <- assign_subsets(num_binom_incomplete, subset_indices)
  
  offset <- num_norm_incomplete
  
  for (i in seq_len(num_norm_incomplete)) {
    idx <- norm_subsets[[i]]
    incomplete_norm[, i] <- complete_data[, idx] %*% B[, i] + rnorm(n)
    covariates_norm[, i] <- colnames(complete_data)[idx]
  }
  
  for (i in seq_len(num_binom_incomplete)) {
    idx <- binom_subsets[[i]]
    linear_pred <- complete_data[, idx] %*% B[, offset + i] + rnorm(n)
    incomplete_binom[, i] <- rbinom(n, 1, 1 / (1 + exp(-linear_pred)))
    covariates_binom[, i] <- colnames(complete_data)[idx]
  }
  
  incomplete_data <- cbind(incomplete_norm, incomplete_binom)
  colnames(incomplete_data) <- paste0("ic_x", 1:ncol(incomplete_data))
  
  covariates <- cbind(covariates_norm, covariates_binom)
  colnames(covariates) <- colnames(incomplete_data)
  
  data <- cbind(complete_data, incomplete_data)
  data.obj <- list(data = data, covariates = covariates)
  return(data.obj)
}


simulate.scenario <- function(K, n, p, s, v, baseline_params, h, eta, cluster_sizes, 
                              simulate_clusters = FALSE, 
                              complete_data_type = c(norm = 1, binom = 0),
                              incomplete_data_type = c(norm = 1, binom = 0)) {
  tasks = list()
  
  if (!simulate_clusters) {
    for (k in 1:cluster_sizes[1]) {
      B = simulate_task(baseline_params[[1]], h, eta, n, v, p - s)
      cat("task:",k,"\n")
      print(B)
      data = generate_data(n, p, s, v, B = B, 
                           complete_data_type,incomplete_data_type)
      tasks[[k]] = list("data" = data[["data"]], "B" = B, "covariates" = data[["covariates"]])
    }
  } else {
    if (sum(cluster_sizes) == K) {
      current_cluster = 1
      current_cluster_limit = cluster_sizes[1]
      for (k in 1:K) {
        if (k > current_cluster_limit && current_cluster < length(cluster_sizes)) {
          current_cluster <- current_cluster + 1
          current_cluster_limit <- current_cluster_limit + cluster_sizes[current_cluster]
        }
        cat("task:",k,"\n")
        cat("cluster:",current_cluster)
        B <- simulate_task(baseline_params[[current_cluster]], h, eta, n, v, p - s)
        print(B)
        data <- generate_data(n, p, s,v, B = B, complete_data_type,incomplete_data_type)
        tasks[[k]] <- list("data" = data[["data"]], "B" = B, "covariates" = data[["covariates"]])
      }
    } else {
      stop("K != cluster_sizes")
    }
  }
  
  return(tasks)
}