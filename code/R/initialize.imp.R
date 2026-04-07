#' Initialize Imputations by Simple Sampling
#'
#' @description
#' Fills missing entries task-by-task by sampling observed values from the same variable.
#' Used to create a non-degenerate starting state for iterative procedures.
#'
#' @param data List of task datasets (\code{data.frame} or tibble per task).
#' @param miss.vars Logical matrix (variables by tasks) marking which variables have missingness.
#' @param where List of logical matrices indicating missing cells within each task.
#'
#' @return A list of the same length as \code{data} with missing values replaced by initial draws.
#'
#' @examples
#' imp0 <- initialize.imp(data, miss.vars, where)
#' @export
#' 

initialize.imp <- function(data, miss.vars, where) {
  imp <- vector("list", length(data))
  
  for (i in 1:length(data)) {
    r <- !is.na(data[[i]])
    imp[[i]] <- data[[i]]
    
    for (j in which(miss.vars[, i])) {
      y <- unlist(data[[i]][, j])
      ry <- r[, j]
      wy <- where[[i]][, j]
      
      # Ensure `y[ry]` is not empty
      valid_values <- y[ry]
      if (length(valid_values) > 0) {
        imp[[i]][wy, j] <- sample(valid_values, size = sum(wy), replace = TRUE)
      } else {
        warning(paste("No valid values to impute for variable", j, "in dataset", i))
      }
    }
  }
  imp
}
