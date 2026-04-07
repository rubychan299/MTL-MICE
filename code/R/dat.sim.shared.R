#' Shared-Covariate Task Simulation Helpers
#'
#' These helpers mirror the originals in `dat.sim.R` but keep the covariate
#' mapping from complete variables to each incomplete variable *identical*
#' across tasks. This makes pooled methods fairer by eliminating task-to-task
#' differences in which predictors drive each incomplete target.
#'
#' @importFrom stats rnorm rbinom runif
#' @import MASS

# Internal: build a covariate mapping once and reuse it across tasks
build_covariate_map <- function(p, s, v,
                                complete_data_type = c(norm = 1, binom = 0),
                                incomplete_data_type = c(norm = 1, binom = 0)) {
  complete_data_type <- complete_data_type / sum(complete_data_type)
  incomplete_data_type <- incomplete_data_type / sum(incomplete_data_type)

  num_norm_complete <- round(s * complete_data_type["norm"])
  num_binom_complete <- round(s * complete_data_type["binom"])

  num_norm_incomplete <- round((p - s) * incomplete_data_type["norm"])
  num_binom_incomplete <- round((p - s) * incomplete_data_type["binom"])

  total_subsets <- floor(s / v)
  usable_cols <- total_subsets * v
  subset_indices <- split(seq_len(usable_cols), rep(1:total_subsets, each = v))

  assign_subsets <- function(num_vars, subsets) {
    total_subsets <- length(subsets)
    if (num_vars <= total_subsets) {
      selected_subsets <- sample(subsets, num_vars, replace = FALSE)
    } else {
      full_cycles <- num_vars %/% total_subsets
      remainder <- num_vars %% total_subsets
      selected_subsets <- rep(subsets, full_cycles)
      if (remainder > 0) {
        selected_subsets <- c(selected_subsets, sample(subsets, remainder, replace = FALSE))
      }
    }
    selected_subsets
  }

  list(
    norm_subsets = assign_subsets(num_norm_incomplete, subset_indices),
    binom_subsets = assign_subsets(num_binom_incomplete, subset_indices),
    num_norm_complete = num_norm_complete,
    num_binom_complete = num_binom_complete,
    num_norm_incomplete = num_norm_incomplete,
    num_binom_incomplete = num_binom_incomplete
  )
}

#' Generate Synthetic Data With Shared Covariate Mapping
#'
#' @inheritParams generate_data
#' @param covariate_map Optional mapping produced by `build_covariate_map`.
#'   If `NULL`, a mapping consistent with the inputs is created once.
#'
#' @return A list with `data`, `B`, and `covariates`, matching the structure of
#'   `generate_data` but using the same covariate mapping for every task.
#' @export
generate_data_shared <- function(n = 100, p = 40, s = 35, v = 10, B,
                                 complete_data_type = c(norm = 1, binom = 0),
                                 incomplete_data_type = c(norm = 1, binom = 0),
                                 covariate_map = NULL) {

  if (is.null(covariate_map)) {
    covariate_map <- build_covariate_map(p, s, v, complete_data_type, incomplete_data_type)
  }

  # Complete data generation mirrors generate_data()
  Sigma <- outer(1:covariate_map$num_norm_complete, 1:covariate_map$num_norm_complete, function(x, y) {
    0.5^(abs(x - y))
  })
  eps <- rnorm(covariate_map$num_norm_complete, sd = 0.3)
  Sigma <- Sigma + eps %*% t(eps)
  R <- chol(Sigma)

  complete_norm <- matrix(rnorm(n * covariate_map$num_norm_complete), nrow = n) %*% R
  complete_binom <- matrix(rbinom(n * covariate_map$num_binom_complete, 1, 0.5), nrow = n)

  complete_data <- cbind(complete_norm, complete_binom)
  colnames(complete_data) <- paste0("c_x", 1:ncol(complete_data))

  incomplete_norm <- matrix(0, n, covariate_map$num_norm_incomplete)
  incomplete_binom <- matrix(0, n, covariate_map$num_binom_incomplete)
  covariates_norm <- matrix(NA, v, covariate_map$num_norm_incomplete)
  covariates_binom <- matrix(NA, v, covariate_map$num_binom_incomplete)

  offset <- covariate_map$num_norm_incomplete

  for (i in seq_len(covariate_map$num_norm_incomplete)) {
    idx <- covariate_map$norm_subsets[[i]]
    incomplete_norm[, i] <- complete_data[, idx, drop = FALSE] %*% B[, i] + rnorm(n)
    covariates_norm[, i] <- colnames(complete_data)[idx]
  }

  for (i in seq_len(covariate_map$num_binom_incomplete)) {
    idx <- covariate_map$binom_subsets[[i]]
    linear_pred <- complete_data[, idx, drop = FALSE] %*% B[, offset + i] + rnorm(n)
    incomplete_binom[, i] <- rbinom(n, 1, 1 / (1 + exp(-linear_pred)))
    covariates_binom[, i] <- colnames(complete_data)[idx]
  }

  incomplete_data <- cbind(incomplete_norm, incomplete_binom)
  colnames(incomplete_data) <- paste0("ic_x", 1:ncol(incomplete_data))

  covariates <- cbind(covariates_norm, covariates_binom)
  colnames(covariates) <- colnames(incomplete_data)

  data <- cbind(complete_data, incomplete_data)
  list(data = data, B = B, covariates = covariates, covariate_map = covariate_map)
}

#' Simulate a Scenario With Shared Covariate Mapping Across Tasks
#'
#' @inheritParams simulate.scenario
#' @param covariate_map Optional mapping from complete variables to incomplete
#'   variables. If omitted, one mapping is created and reused for all tasks.
#'
#' @return A list of tasks where each task shares the same covariate mapping.
#' @export
simulate.scenario.shared <- function(K, n, p, s, v, baseline_params, h, eta, cluster_sizes,
                                     simulate_clusters = FALSE,
                                     complete_data_type = c(norm = 1, binom = 0),
                                     incomplete_data_type = c(norm = 1, binom = 0),
                                     covariate_map = NULL) {

  tasks <- list()
  if (is.null(covariate_map)) {
    covariate_map <- build_covariate_map(p, s, v, complete_data_type, incomplete_data_type)
  }

  if (!simulate_clusters) {
    for (k in 1:cluster_sizes[1]) {
      B <- simulate_task(baseline_params[[1]], h, eta, n, v, p - s)
      data <- generate_data_shared(
        n, p, s, v, B = B,
        complete_data_type = complete_data_type,
        incomplete_data_type = incomplete_data_type,
        covariate_map = covariate_map
      )
      tasks[[k]] <- list("data" = data[["data"]], "B" = B, "covariates" = data[["covariates"]])
    }
  } else {
    if (sum(cluster_sizes) == K) {
      current_cluster <- 1
      current_cluster_limit <- cluster_sizes[1]
      for (k in 1:K) {
        if (k > current_cluster_limit && current_cluster < length(cluster_sizes)) {
          current_cluster <- current_cluster + 1
          current_cluster_limit <- current_cluster_limit + cluster_sizes[current_cluster]
        }
        B <- simulate_task(baseline_params[[current_cluster]], h, eta, n, v, p - s)
        data <- generate_data_shared(
          n, p, s, v, B = B,
          complete_data_type = complete_data_type,
          incomplete_data_type = incomplete_data_type,
          covariate_map = covariate_map
        )
        tasks[[k]] <- list("data" = data[["data"]], "B" = B, "covariates" = data[["covariates"]])
      }
    } else {
      stop("K != cluster_sizes")
    }
  }

  return(tasks)
}
