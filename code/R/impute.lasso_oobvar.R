#' Single-Iteration Multi-Task LASSO Imputation (Internal)
#'
#' @description
#' Performs one full iteration of multi-task LASSO-based imputation across all tasks
#' and all variables flagged as incomplete.
#'
#' @param data List of original task datasets.
#' @param dat.imp List of the current working copies with imputed values.
#' @param maxit Integer. Current iteration index (or total iterations, depending on caller).
#' @param miss.vars Logical matrix (rows = variables, cols = tasks) indicating which
#'   variables are incomplete in each task.
#' @param where List of logical matrices (\code{n_i} by \code{p}) with \code{TRUE} where values are missing.
#' @param transfer.id.mode Character. Transfer identifier mode, typically \code{"auto"}, \code{"all"}, or \code{"none"}.
#' @param lambda.transfer,lambda.debias,lambda.detection See \code{\link{mtlmice}}.
#' @param correction Logical. Apply coefficient correction where applicable.
#' @param cores Integer. Number of workers for parallel fits.
#' @param ... Additional arguments passed to \pkg{glmnet} or internal helpers.
#'
#' @return A list with class \code{"mtlmice"} containing (at least):
#' \itemize{
#'   \item \code{imp}: updated per-iteration imputations.
#'   \item \code{dat.full}: list of updated datasets with imputed values.
#'   \item \code{norm.ind}: per-task indicators for treated-as-Gaussian variables.
#'   \item \code{multi.task}: per-task, per-variable model objects and coefficients.
#'   \item \code{chainMean}, \code{chainVar}: diagnostic summaries of predictions per iteration.
#' }
#'
#' @note This routine is designed to be called by \code{\link{mtlmice}} rather than directly by users.
#' @keywords internal
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom glmtrans glmtrans source_detection
#' @importFrom stats as.formula rnorm runif
#' @importFrom utils capture.output
#' @importFrom parallel detectCores
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom tidyr drop_na
#' @importFrom dplyr select
#' 
#' Column Type Heuristic (Internal)
#'
#' @description
#' Returns a logical vector marking columns with more than two unique non-missing
#' values, typically treated as continuous in modeling steps.
#'
#' @param x A data.frame or matrix.
#' @return Logical vector of length \code{ncol(x)}.
#' @keywords internal
#' @noRd
type <- function(x) {
  vapply(x, function(column) {
    length(na.omit(unique(column))) > 2
  }, numeric(1))
}

#' Negated \code{\%in\%} (Internal)
#' @keywords internal
#' @return A logical vector, the negation of \code{\%in\%}.
#' @name notin
#' @usage x \%notin\% table
#' @noRd
#' 
`%notin%` <- Negate(`%in%`)


impute.lasso <- function(data, dat.imp, maxit, miss.vars, where, nfolds = 10, transfer.id.mode = "auto", lambda.transfer = "lambda.1se",
                            lambda.debias = "lambda.min", lambda.detection = "lambda.1se", correction = TRUE,cores = 1,...) {
  
  # creating empty objects to store the values
  dat.full <- dat.imp
  norm.ind <- vector("list", length(data))
  multitaski <- vector("list", length(data))
  imp <- vector("list", length(data))
  impi <- vector("list", maxit)
  impmaxit <- vector("list", nrow(miss.vars))
  names(impmaxit) <- rownames(miss.vars)
  names(impi) <- seq_len(maxit)
  chainMean <- chainVar <- initialize.chain(data, miss.vars, maxit)
  k <- NULL
  
  # the mit-level iteration
  for (mit in 1:maxit) {
    
    print(paste0("Iteration: ", mit))
    
    # loop through all tasks
    for (i in 1:length(data)) {
      datai <- data[[i]] # i-th task
      r <- !is.na(datai)
      norm.indi <- apply(datai, 2, function(x) {
        length(na.omit(unique(x))) > 2
      })
      source.norm.idi <- lapply(data, function(x) type(x))
      source <- dat.full[-i]
      target <- dat.full[[i]]
      
      # loop through all columns in each task
      for (j in which(miss.vars[, i])) { ## impute j-th column of i-th task
        
        # the target location to impute(y with missing)
        target.wy <- where[[i]][, j] # impute data[target.wy, j]
        # the location on sources for modeling (x)
        source.w <- where[-i]
        # the target y without missing
        target.ry <- r[, j]
        # the name of the target column
        namej <- colnames(datai)[j]
        
        # nothing to impute
        if (all(!target.wy)) {
          return(numeric(0))
        }
        
        
        # Source detection
        if (norm.indi[j]) {
          family <- "gaussian"
        } else {
          family <- "binomial"
        }
        
        if (is.null(transfer.id.mode) || (transfer.id.mode == "all") || (transfer.id.mode == "none_all")) { # transfer all source data
          transfer.id <- 1:length(source)
        } else if (transfer.id.mode == "auto") { # automatically check which source data set to transfer
          target.dect <- list(x = target[, -j], y = target[, j])
          source.dect <- source
          for (l in 1:length(source.dect)) {
            source.dect[[l]] <- list(x = source.dect[[l]][, -j], y = source.dect[[l]][, j])
          }
          A <- glmtrans::source_detection(
            target = target.dect, family = family, source = source.dect, alpha = 1, standardize = TRUE,
            intercept = TRUE, nfolds = nfolds, epsilon0 = 0.01, cores = 1, valid.proportion = NULL, valid.nfolds = 3,
            lambda = lambda.detection, detection.info = TRUE, target.weights = NULL, source.weights = NULL, C0 = 2
          )
          transfer.id <- A$transfer.source.id
          
          
        } else if (transfer.id.mode == "none") { # don't transfer any source
          transfer.id <- 0
        }
        
        num_missing <- sum(target.wy)
        if (num_missing > 1) {
          x.glmnet <- cbind(1, target[target.wy, -j])
          y.glmnet <- target[target.wy, j]
        } else {
          x.glmnet <- cbind(1, t(target[target.wy, -j, drop = FALSE]))
          y.glmnet <- target[target.wy, j]
        }
        
        
        # the initial imputed target x and y where y is not missing
        target.x.imp <- target[!target.wy, -j]
        target.y.imp <- target[!target.wy, j]
        
        
        # Bootstrap sample
        
        ## target
        
        target.n1 <- sum(target.ry)
        
        target.s <- sample(target.n1, target.n1, replace = TRUE)
        
        target.x <- target.x.imp[target.s, , drop = FALSE]
        target.y <- target.y.imp[target.s]
        
        ## source
        
        source.n1 <- NULL
        source.s <- NULL
        source.x <- NULL
        source.y <- NULL
        
        for (k in 1:length(source)) {
          source.r <- !is.na(data[-i][[k]])
          source.ry <- source.r[, j]
          source.wy <- source.w[[k]][, j]
          
          source.x.imp <- source[[k]][!source.wy, -j]
          source.y.imp <- source[[k]][!source.wy, j]
          
          source.n1 <- sum(source.ry)
          
          source.s <- sample(source.n1, source.n1, replace = TRUE)
          
          source.x[[k]] <- source.x.imp[source.s, , drop = FALSE]
          source.y[[k]] <- source.y.imp[source.s]
        }
        
        if (transfer.id.mode == "none")  {
          # non-transfer
          fit <- glmnet::cv.glmnet(
            x = target.x, y = target.y, family = family, alpha = 1, nfolds = nfolds,
            parallel = I(cores > 1)
          )
          
          coeff_mat <- coef(fit, s = "lambda.min") # Use lambda.debias for consistency
          coeff <- as.numeric(coeff_mat)
          names(coeff) <- rownames(coeff_mat)
          coeff.corr <- rep(0, length(coeff))
          beta.hat <- coeff
          
        } else if (transfer.id.mode == "none_all"){          
          # non-transfer pooled
          all.x <- rbind(target.x, do.call(rbind, source.x))
          all.y <- c(target.y, unlist(source.y))

          fit <- glmnet::cv.glmnet(
            x = all.x, y = all.y, family = family, alpha = 1, nfolds = nfolds,
            parallel = I(cores > 1)
          )
          
          coeff_mat <- coef(fit, s = "lambda.min") # Use lambda.debias for consistency
          coeff <- as.numeric(coeff_mat)
          names(coeff) <- rownames(coeff_mat)
          coeff.corr <- rep(0, length(coeff))
          beta.hat <- coeff
          
        } else if (is.null(transfer.id.mode) || (transfer.id.mode == "all") || (transfer.id.mode == "auto") || (length(transfer.id) != 0)){
                    
          target.fit <- list(x = target.x, y = target.y)
          source.fit <- lapply(1:length(source), function(k) list(x = source.x[[k]], y = source.y[[k]]))
          fit <- glmtrans(target.fit, source.fit, family = family, transfer.source.id = transfer.id,
                          lambda = c(transfer = lambda.transfer, debias = lambda.debias, detection = lambda.detection),
                          cores = cores)
          
          coeff <- fit[["fitting.list"]][["w_a"]]
          beta.hat <- fit$beta
          coeff.corr <- fit[["fitting.list"]][["delta_a"]]
          names(beta.hat) <- c("intercept", colnames(target[, -j]))
          names(coeff)    <- c("intercept", colnames(target[, -j])) 
        }
        
        # Obtain imputations
        
        nfit <- as.matrix(x.glmnet %*% coeff)
        nfit.pred <- as.matrix(x.glmnet %*% beta.hat)    
        
        if (norm.indi[j]) {
           
          if (correction) {
            beta <- beta.hat
            # OOB error: .632 bootstrap estimate ----

            # In-bag (training) MSE
            nfit.boot <- as.matrix(cbind(1,target.x) %*% beta)
            s2_in <- mean((nfit.boot - target.y)^2)

            # OOB indices (relative to target.x.imp / target.y.imp)
            oob <- setdiff(seq_len(target.n1), unique(target.s))

            if (length(oob) >= 10) {
            x_oob <- cbind(1, target.x.imp[oob, , drop = FALSE])
            y_oob <- target.y.imp[oob]

            yhat_oob <- as.matrix(x_oob %*% beta)
            s2_oob <- mean((yhat_oob - y_oob)^2)

            # .632 blend (fixed weights; no manual tuning)
            s2hat <- 0.368 * s2_in + 0.632 * s2_oob
            } else {
            # fallback if OOB is too small (rare unless n is tiny)
            s2hat <- s2_in
            }
            pred.y <- as.vector(nfit.pred) + rnorm(sum(target.wy), 0, sqrt(s2hat))
          }else{
            beta <- coeff

            # OOB error: .632 bootstrap estimate ----

            # In-bag (training) MSE
            nfit.boot <- as.matrix(cbind(1,target.x) %*% beta)
            s2_in <- mean((nfit.boot - target.y)^2)

            # OOB indices (relative to target.x.imp / target.y.imp)
            oob <- setdiff(seq_len(target.n1), unique(target.s))

            if (length(oob) >= 10) {
              x_oob <- cbind(1, target.x.imp[oob, , drop = FALSE])
              y_oob <- target.y.imp[oob]

              yhat_oob <- as.matrix(x_oob %*% beta)
              s2_oob <- mean((yhat_oob - y_oob)^2)

              # .632 blend (fixed weights; no manual tuning)
              s2hat <- 0.368 * s2_in + 0.632 * s2_oob
            } else {
            # fallback if OOB is too small (rare unless n is tiny)
            s2hat <- s2_in
            }
            pred.y <- as.vector(nfit) + rnorm(sum(target.wy), 0, sqrt(s2hat))
          }
          
          dat.full[[i]][target.wy, j] <- pred.y
          impmaxit[[j]] <- pred.y
          
        } else if (!norm.indi[j]){
          
          
          nfit.pred.y <- as.numeric(nfit.pred > 0)
          
          nfit.y <- as.numeric(nfit > 0)
          
          if(correction){
            beta <- beta.hat
            p <- 1 / (1 + exp(-nfit.pred))
          }else{
            beta <- coeff
            p <- 1 / (1 + exp(-nfit))
          }

            vec <- (runif(nrow(p)) <= p)
            vec[vec] <- 1
            pred.y <- vec

            dat.full[[i]][target.wy, j] <- pred.y
            impmaxit[[j]] <- pred.y
        }
        
        impi[[mit]] <- impmaxit
        imp[[i]] <- impi
        
        chainMean[[i]][j, mit] <- mean(pred.y)
        chainVar[[i]][j, mit] <- var(pred.y)
        
        
        
        multitaski[[i]][[j]] <- list(
          beta = beta,
          family = family,
          transfer.id = transfer.id,
          fitting.list = list(coeff = coeff, coeff.correct = coeff.corr)
        )
        }
      
      norm.ind[[i]] <- norm.indi
      
      output <- list(imp = imp,
                     dat.full = dat.full,
                     norm.ind = norm.ind,
                     multi.task = multitaski,
                     chainMean = chainMean,
                     chainVar = chainVar
      )
    }
  }
  
  class(output) <- "mtlmice"
  return(output)
}
