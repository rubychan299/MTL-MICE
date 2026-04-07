#' Initialize Chain Containers for Diagnostics
#'
#' @description
#' Allocates per-task matrices to store iteration-wise summaries (e.g., means/variances)
#' for each incomplete variable across \code{maxit} iterations.
#'
#' @param data List of task datasets.
#' @param miss.vars Logical matrix with row names equal to variable names.
#' @param maxit Integer. Number of iterations to allocate.
#'
#' @return A list of length \code{length(data)}; each element is a \code{length(vars) x maxit}
#'   \code{matrix} with dimnames set to variable names and iteration indices.
#'
#' @examples
#' ch <- initialize.chain(data, miss.vars, maxit = 5)
#' dim(ch[[1]])
#' @export
#' 
initialize.chain <- function(data, miss.vars, maxit) {
  
  
  chain <- vector("list", length(data))
  for(i in 1:length(data)){
    vars <- rownames(miss.vars)
    chain[[i]] <- array(NA, dim = c(length(vars), maxit))
    dimnames(chain[[i]]) <- list(
      vars,
      seq_len(maxit)
    )
    chain
  }
  return(chain)
}

