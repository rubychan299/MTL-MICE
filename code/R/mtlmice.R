#' Multi-Task Multiple Imputation by Chained Equations (MTL-MICE) for Incomplete Multi-Task Data
#'
#' @description
#' Runs an iterative multiple-imputation procedure for a *list* of related
#' datasets (tasks), using multi-task LASSO for prediction and optional
#' transfer-identifier selection. Returns imputed datasets, fitted models,
#' and diagnostic chains.
#'
#' @param data A \code{list} of data.frames or tibbles, one per task. Each
#'   element must share column names; rows are task-specific.
#' @param m Integer. Number of multiply imputed datasets to generate. Default \code{5}.
#' @param maxit Integer. Number of Gibbs-style iterations per imputed dataset. Default \code{5}.
#' @param nfolds Integer. Number of folds for internal cross-validation in \pkg{glmnet}. Default \code{10}.
#' @param lambda.transfer Character or numeric. How to pick the transfer-identifier
#'   penalty; typically \code{"lambda.1se"} or \code{"lambda.min"}. Default \code{"lambda.1se"}.
#' @param lambda.debias Character or numeric. Penalty used for the debias/refit step
#'   after variable selection. Default \code{"lambda.min"}.
#' @param lambda.detection Character or numeric. Penalty used for detection/selection
#'   of transfer features. Default \code{"lambda.1se"}.
#' @param transfer.id.mode One of \code{c("all","auto","none")}. Controls how transfer
#'   identifiers are used across tasks. Default \code{"all"}.
#' @param cores Integer. Number of parallel workers used via \pkg{doParallel}. Default \code{1}.
#* @param correction Logical. If \code{TRUE}, applies small-sample or shrinkage
#'   corrections to coefficient estimates where applicable. Default \code{TRUE}.
#' @param ... Passed to lower-level fitting routines if supported.
#'
#' @details
#' The function expects \code{data} to be a list with consistent variable names.
#' Missingness is imputed variable-by-variable using task-coupled LASSO fits.
#' The result stores both initial and final imputations, fitted model artifacts,
#' and per-iteration chain summaries.
#'
#' @return
#' An object of class \code{"mtlmice"} with components:
#' \itemize{
#'   \item \code{data}: original list of task datasets.
#'   \item \code{where}: list of logical matrices indicating missing entries.
#'   \item \code{imp}: raw imputation draws by iteration and task.
#'   \item \code{dat.imp.init}: list of initial imputations for each \code{m}.
#'   \item \code{dat.imp.final}: list of final imputations for each \code{m}.
#'   \item \code{models}: fitted multi-task model artifacts.
#'   \item \code{chain}: per-task matrices with iteration-wise summaries.
#'   \item \code{lastSeedValue}: RNG state for reproducibility.
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' tasks <- list(
#'   A = data.frame(x=rnorm(100), y=rnorm(100)),
#'   B = data.frame(x=rnorm(120), y=rnorm(120))
#' )
#' tasks$A$y[sample(100, 20)] <- NA
#' fit <- mtlmice(tasks, m = 3, maxit = 5, cores = 1)
#' class(fit)
#' }
#'
#' @import foreach glmnet doParallel
#' @export

library(foreach)
library(glmnet)
library(doParallel) 

mtlmice <- function(data, m = 5, maxit = 5, nfolds = 10, lambda.transfer = "lambda.1se",
                    lambda.debias = "lambda.min", lambda.detection = "lambda.1se", 
                    transfer.id.mode = c("all", "auto", "none", "none_all"),
                    cores = 1, correction = TRUE,
                    ...) {
  
  transfer.id.mode <- match.arg(transfer.id.mode)
  
  if (!is.list(data)) {
    stop("Error: Input must be a list.")
  }
  
  data <- lapply(data, function(obj) {
    if (is.data.frame(obj)) {
      return(as.matrix(obj))
    } else if (is.matrix(obj)) {
      return(obj)
    } else {
      stop("Error: Objects within the list must be either matrices or data frames.")
    }
  })
  
  miss.vars <- sapply(data, function(x) {
    apply(is.na(x), 2, sum) > 0
  })
  where <- lapply(data, function(x) {
    is.na(x)
  })
  
  dat.imp.init <- initialize.imp(data, miss.vars, where)
  
  if (cores > 1) {
    cl <- makeCluster(cores)
    registerDoParallel(cl)
  }
  
  # Export the objects we actually create in this scope
  needed_objects <- c("data", "impute.lasso", "type", 
                      "%notin%", "initialize.chain", "initialize.imp",
                      "dat.imp.init", "miss.vars", "where")

  `%foreach_operator%` <- if (cores > 1) `%dopar%` else `%do%`
  
  obj <- foreach(
    h = 1:m,
    .export = needed_objects,
    .combine = rbind,
    .packages = c("glmnet", "glmtrans") 
  ) %foreach_operator% {
    
    result <- impute.lasso(
      data = data, dat.imp = dat.imp.init, maxit = maxit, 
      miss.vars = miss.vars, where = where, 
      transfer.id.mode = transfer.id.mode,
      nfolds = nfolds, lambda.transfer = lambda.transfer,
      lambda.debias = lambda.debias, lambda.detection = lambda.detection,
      cores = cores, correction = correction,
      ...
    )
    
    # Return the result for this iteration
    result
  }
  
  # Unpack results from the combined matrix/data.frame/list
  if (is.list(obj) && !is.data.frame(obj) && (m == 1)) {
    # Single imputation: obj is a named list
    imp <- list(obj$imp)
    dat.imp.final <- list(obj$dat.full)
    multi.task <- list(obj$multi.task)
    chain <- list(
      chainMean = list(obj$chainMean),
      chainVar = list(obj$chainVar)
    )
  } else {
    imp <- obj[, "imp"]
    dat.imp.final <- obj[, "dat.full"]
    multi.task <- obj[, "multi.task"]
    chain <- list(
      chainMean = obj[, "chainMean"],
      chainVar = obj[, "chainVar"]
    )
  }
  
  if (cores > 1) {
    stopCluster(cl)
    registerDoSEQ()
  }
  
  mtlmice.obj <- list(
    data = data,
    where = where,
    imp = imp,
    dat.imp.init = dat.imp.init,
    dat.imp.final = dat.imp.final,
    models = multi.task,
    chain = chain,
    lastSeedValue = get(".Random.seed", envir = globalenv(), mode = "integer", inherits = FALSE)
  )
  class(mtlmice.obj) <- "mtlmice"
  return(mtlmice.obj)
}
