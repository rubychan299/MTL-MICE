summarize_mtlmice_pool <- function(output_imp, method, iterations = 100) {
  library(dplyr)
  library(purrr)
  
  # Flatten and tag the data
  combined <- map_dfr(output_imp, function(x) {
    if (is.null(x)) return(tibble())
    
    rows <- list()
    if (!is.null(x$Continuous)) {
      rows[[length(rows) + 1]] <- x$Continuous %>% mutate(type_group = "Continuous")
    }
    if (!is.null(x$Categorical)) {
      rows[[length(rows) + 1]] <- x$Categorical %>% mutate(type_group = "Categorical")
    }
    if (!is.null(x$Coefficients)) {
      rows[[length(rows) + 1]] <- x$Coefficients %>% mutate(type_group = "Coefficients")
    }
    
    if (length(rows) == 0) return(tibble())
    bind_rows(rows)
  })
  
  # Helper function to summarize by type
  summarize_by_type <- function(data, type_label) {
    data %>%
      filter(type_group == type_label) %>%
      group_by(Task, Variable, type_group) %>%
      summarise(
        across(where(is.numeric), list(mean = ~mean(.x[is.finite(.x)], na.rm = TRUE),
                                       se = ~sd(.x[is.finite(.x)], na.rm = TRUE) / sqrt(iterations)),
               .names = "{.fn}_{.col}"),
        .groups = "drop"
      ) %>%
      mutate(method = method) %>%
      dplyr::select(where(~ !all(is.na(.))))
  }
  
  # Return both summaries
  list(
    Continuous = summarize_by_type(combined, "Continuous"),
    Categorical = summarize_by_type(combined, "Categorical"),
    Coefficients = summarize_by_type(combined, "Coefficients")
  )
}
