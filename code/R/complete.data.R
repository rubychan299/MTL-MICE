#' Extract Completed Data From an \code{mtlmice} Fit
#'
#' @description
#' Returns stacked or mild-stacked completed datasets from an \code{mtlmice} object.
#'
#' @param imp An \code{mtlmice} object returned by \code{\link{mtlmice}}.
#' @param action Either a character mode (\code{"stacked"} or \code{"mild"}) or an
#'   integer vector specifying which imputed datasets to return.
#' @param include Logical. If \code{TRUE} and \code{action} is numeric, include the first
#'   completed dataset in addition to the requested indices. Default \code{TRUE}.
#' @param mild Logical. If \code{TRUE}, return the "mild" stacked structure (if supported).
#'
#' @details
#' For \code{action = "stacked"}, returns a tibble with columns \code{.imp}, \code{.id},
#' followed by the variables. For numeric \code{action}, returns only the selected sets.
#'
#' @return A tibble (or list of tibbles) of completed data, depending on \code{action}.
#'
#' @examples
#' \dontrun{
#' cmp <- complete.data(fit, action = "stacked")
#' head(cmp[[1]])
#' }
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble

library(tibble)

complete.data <- function(imp, action = "stacked", include = TRUE, mild = FALSE) {
  
  data <- imp$dat.imp.final  # List of imputed datasets
  original <- imp$data       # Original dataset
  
  m <- length(data)  # Number of imputed datasets
  
  if (is.numeric(action)) {
    action <- as.integer(action)
    idx <- action[action >= 1L & action <= m]  # Ensure valid indices
    if (include) idx <- unique(c(1L, idx))  # Include first dataset if needed
    shape <- ifelse(mild, "mild", "stacked")
  } else if (is.character(action)) {
    if (include) idx <- 1L:m else idx <- 1L:m  # Avoid `0`
    shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
    shape <- ifelse(shape == "all" || mild, "mild", shape)
  } else {
    stop("'action' not recognized")
  }
  
  # Ensure idx contains only valid indices
  idx <- idx[idx > 0 & idx <= m]
  
  if (length(idx) == 0) stop("Error: No valid imputation indices selected.")
  
  # Create list to store selected imputations
  mylist <- vector("list", length = length(idx))
  for (j in seq_along(idx)) {
    mylist[[j]] <- data[[idx[j]]]
  }
  
  # --- Stacked Format ---
  if (shape == "stacked") {
    return(lapply(1:length(original), function(i) {
      as_tibble(do.call(rbind, lapply(mylist, function(res) res[[i]])))
    }))
  }
  
  # --- Mild Format ---
  if (shape == "mild") {
    names(mylist) <- as.character(idx)
    class(mylist) <- c("mild", "list")
    return(mylist)
  }
  
  # --- Long Format (Handling Inconsistent Row Counts) ---
  if (shape == "long") {
    cmp <- lapply(1:length(original), function(i) {
      row_counts <- sapply(mylist, function(res) nrow(res[[i]]))
      common_row_count <- as.numeric(names(sort(table(row_counts), decreasing = TRUE)[1]))
      valid_mylist <- lapply(mylist, function(res) {
        if (nrow(res[[i]]) == common_row_count) return(res[[i]])
        else return(NULL)
      })
      valid_mylist <- valid_mylist[!sapply(valid_mylist, is.null)]
      as_tibble(do.call(rbind, valid_mylist))
    })
    
    cmp <- lapply(seq_along(cmp), function(i) {
      cmp_df <- as_tibble(data.frame(
        .imp = rep(seq_along(cmp[[i]]), each = nrow(cmp[[i]])),
        .id = rep.int(1L:nrow(cmp[[i]]), length(cmp[[i]])),
        cmp[[i]]
      ))
      return(cmp_df)
    })
    
    return(cmp)
  }
  
  return(NULL)  # Default return if no valid action is found
}
