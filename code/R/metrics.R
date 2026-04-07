#' @importFrom dplyr bind_rows group_by summarise mutate across ungroup
library(dplyr)

#' Evaluate Imputation Accuracy on Held-Out Cells
#'
#' @description
#' Compares imputed values against the original values at missing positions, returning
#' summary metrics per variable and type (continuous/binary).
#'
#' @param raw_data Data.frame with the original (pre-missing) values.
#' @param imputed_data Data.frame after imputation.
#' @param where Logical data.frame/matrix with `TRUE` at cells that were missing.
#'
#' @return A list with elements `Continuous_Summary` and/or `Categorical_Summary`,
#' or `NULL` if no missing values were evaluated.
#' @export
evaluate.imputation <- function(raw_data, imputed_data, where) {
  
  raw_data <- as.data.frame(raw_data)
  imputed_data <- as.data.frame(imputed_data)
  where <- as.data.frame(where)
  
  continuous_results <- list()
  categorical_results <- list()
  
  for (col in colnames(raw_data)) {
    missing_idx <- which(where[, col], arr.ind = TRUE)
    if (length(missing_idx) == 0) next
    
    original_values <- raw_data[[col]][missing_idx]
    imputed_values <- imputed_data[[col]][missing_idx]
    
    valid_idx <- !is.na(original_values)
    if (sum(valid_idx) == 0) next
    original_values <- original_values[valid_idx]
    imputed_values <- imputed_values[valid_idx]
    
    is_continuous <- length(na.omit(unique(imputed_data[[col]]))) > 2
    
    if (is_continuous) {
      mse <- mean((original_values - imputed_values)^2, na.rm = TRUE)
      var_original <- var(original_values, na.rm = TRUE)
      mse_rel <- if (!is.na(var_original) && var_original > 0) mse / var_original else NA
      
      rmse_val <- sqrt(mse)
      mae_val <- mean(abs(original_values - imputed_values), na.rm = TRUE)
      
      train_mean <- mean(raw_data[[col]][!where[[col]]], na.rm = TRUE)
      rss <- sum((original_values - imputed_values)^2)
      tss <- sum((original_values - train_mean)^2)
      
      r2_out <- if (!is.na(tss) && tss > 0) 1 - rss / tss else NA
      
      continuous_results[[length(continuous_results) + 1]] <- data.frame(
        Variable = col, MSE = mse, RMSE = rmse_val, MSE_rel = mse_rel,
        MAE = mae_val, R2_out = r2_out, stringsAsFactors = FALSE
      )
    } else {
      accuracy <- mean(original_values == imputed_values, na.rm = TRUE)
      categorical_results[[length(categorical_results) + 1]] <- data.frame(
        Variable = col, Accuracy = accuracy, stringsAsFactors = FALSE
      )
    }
  }
  
  output_list <- list()
  if (length(continuous_results) > 0) {
    output_list$Continuous_Summary <- dplyr::bind_rows(continuous_results)
  }
  if (length(categorical_results) > 0) {
    output_list$Categorical_Summary <- dplyr::bind_rows(categorical_results)
  }
  
  return(if (length(output_list) > 0) output_list else NULL)
}

#' Evaluate Coefficient Estimation Accuracy (Internal)
#'
#' @description
#' Compares estimated model coefficients from an mtlmice object against true
#' coefficients provided in a sparse format (`B` and `covariates` matrices).
#'
#' @param models A list of model objects from `imp$models`.
#' @param rawdata The original list of tasks, containing `B` and `covariates`.
#' @param where A list of logical matrices indicating missing cell locations.
#' @param show_warnings Logical. If `TRUE` (default), informational warnings are displayed.
#'
#' @return A data.frame with columns for Task, Variable, Beta_MSE (from the stored
#'   `beta` vector), and Coeff_Bias (from the raw `coeff` vector when available), or `NULL`.
#' @keywords internal
evaluate.coefficients <- function(models, rawdata, where, show_warnings = TRUE) {
  
  miss.vars <- sapply(where, function(x) apply(x, 2, any, na.rm = TRUE))
  if (!is.matrix(miss.vars)) miss.vars <- matrix(miss.vars, ncol = length(where))
  
  num_tasks <- length(where)
  var_names <- colnames(where[[1]])
  m <- length(models)
  results_list <- list()
  
  for (h in 1:m) { # Loop over m imputations
    model_h <- models[[h]]
    for (i in 1:num_tasks) { # Loop over tasks
      if (is.null(rawdata[[i]][["B"]]) || is.null(rawdata[[i]][["covariates"]])) {
        next
      }
      true_coeffs_matrix <- rawdata[[i]][["B"]]
      true_covariates_matrix <- rawdata[[i]][["covariates"]]
      
      for (j in which(miss.vars[, i])) { # Loop over imputed variables
        if (is.null(model_h[[i]][[j]])) next
        
        current_var_name <- var_names[j]
        
        if (current_var_name %in% colnames(true_covariates_matrix)) {
          
          col_idx <- match(current_var_name, colnames(true_covariates_matrix))
          if (is.na(col_idx)) next
          
          # Normalize name conventions to improve alignment
          normalize_names <- function(x) {
            nm <- names(x)
            if (is.null(nm)) return(x)
            nm[nm == "(Intercept)"] <- "intercept"
            names(x) <- nm
            x
          }
          
          beta_hat <- model_h[[i]][[j]]$beta
          coeff_raw <- model_h[[i]][[j]]$fitting.list$coeff

          # Build true coefficient vector aligned to all available names
          true_predictor_names <- na.omit(true_covariates_matrix[, col_idx])
          true_predictor_coeffs <- na.omit(true_coeffs_matrix[, col_idx])
          true_predictor_names[true_predictor_names == "(Intercept)"] <- "intercept"
          if (length(true_predictor_names) != length(true_predictor_coeffs)) {
            if (show_warnings) warning(paste("Unequal number of true covariates and coefficients for task", i, "variable", current_var_name))
            next
          }
          named_true_coeffs <- setNames(true_predictor_coeffs, true_predictor_names)
          
          # If beta_hat/coeff_raw are unnamed, try to infer names from true predictors (plus intercept)
          infer_names <- function(vec) {
            if (!is.null(names(vec))) return(normalize_names(vec))
            if (length(vec) == length(true_predictor_names) + 1) {
              names(vec) <- c("intercept", true_predictor_names)
            } else if (length(vec) == length(true_predictor_names)) {
              names(vec) <- true_predictor_names
            }
            normalize_names(vec)
          }
          beta_hat <- infer_names(beta_hat)
          coeff_raw <- infer_names(coeff_raw)
          
          # If raw coefficients are still unnamed but match beta length, assume same order
          if (is.null(names(coeff_raw)) && !is.null(names(beta_hat)) && length(coeff_raw) == length(beta_hat)) {
            names(coeff_raw) <- names(beta_hat)
            coeff_raw <- normalize_names(coeff_raw)
          }
          
          # Compare only on the true generating support (+ intercept), as in legacy bias/bias.hat
          compare_names <- unique(c("intercept", names(named_true_coeffs)))
          beta_true <- setNames(numeric(length(compare_names)), compare_names)
          beta_true[names(named_true_coeffs)] <- named_true_coeffs
          
          beta_hat_aligned <- setNames(numeric(length(compare_names)), compare_names)
          coeff_raw_aligned <- setNames(numeric(length(compare_names)), compare_names)
          
          overlap_hat <- intersect(names(beta_hat), compare_names)
          if (length(overlap_hat) > 0) beta_hat_aligned[overlap_hat] <- beta_hat[overlap_hat]
          
          if (!is.null(coeff_raw)) {
            overlap_coeff <- intersect(names(coeff_raw), compare_names)
            if (length(overlap_coeff) > 0) coeff_raw_aligned[overlap_coeff] <- coeff_raw[overlap_coeff]
          }
          
          mse_beta <- mean((beta_hat_aligned - beta_true)^2)
          mse_coeff <- mean((coeff_raw_aligned - beta_true)^2)
          
          results_list <- append(results_list, list(data.frame(
            Imputation = h,
            Task = i,
            Variable = current_var_name,
            Beta_MSE = mse_beta,
            Coeff_Bias = mse_coeff
          )))
        }
      }
    }
  }
  
  if (length(results_list) == 0) {
    if (show_warnings) warning("Could not match any imputed variables with provided true coefficients. Check variable and covariate names.")
    return(NULL)
  }
  
  summary_df <- dplyr::bind_rows(results_list) %>%
    dplyr::group_by(Task, Variable) %>%
    dplyr::summarise(
      Beta_MSE = mean(Beta_MSE, na.rm = TRUE),
      Coeff_Bias = mean(Coeff_Bias, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(summary_df)
}


#' Calculate Imputation and Coefficient Error for MTLMICE Output
#'
#' @description
#' Calculates imputation accuracy and coefficient estimation error by comparing the
#' `mtlmice` object against the original, complete data.
#'
#' @param imp An object of class `mtlmice`.
#' @param rawdata A list of the original, complete datasets.
#' @param show_warnings Logical. If `TRUE` (default), informational warnings are displayed.
#'
#' @return A list containing summary data.frames: `Continuous`, `Categorical`,
#'   and `Coefficients`.
error.rate <- function(imp, rawdata, show_warnings = TRUE) {
  
  if (!is.list(rawdata)) stop("Error: rawdata must be a list.")
  
  rawdata_std <- lapply(rawdata, function(obj) {
    if (is.list(obj) && !is.null(obj[["data"]])) {
      obj$data <- as.data.frame(obj$data); return(obj)
    } else if (is.data.frame(obj)) {
      return(list(data = obj))
    } else if (is.matrix(obj)) {
      return(list(data = as.data.frame(obj)))
    }
    stop("Error: Each element in rawdata must be a data.frame, matrix, or a list containing a 'data' element.")
  })
  
  where <- imp$where
  data_imputed_list <- imp$dat.imp.final
  
  # --- 1. Imputation Accuracy ---
  results <- lapply(seq_along(data_imputed_list), function(i) {
    lapply(seq_along(data_imputed_list[[i]]), function(j) {
      eval_res <- evaluate.imputation(rawdata_std[[j]][["data"]], data_imputed_list[[i]][[j]], where[[j]])
      if (!is.null(eval_res)) {
        if (!is.null(eval_res$Continuous_Summary)) eval_res$Continuous_Summary$Task <- j
        if (!is.null(eval_res$Categorical_Summary)) eval_res$Categorical_Summary$Task <- j
      }
      return(eval_res)
    })
  })
  
  all_continuous <- dplyr::bind_rows(lapply(unlist(results, recursive = FALSE), `[[`, "Continuous_Summary"))
  all_categorical <- dplyr::bind_rows(lapply(unlist(results, recursive = FALSE), `[[`, "Categorical_Summary"))
  output <- list()
  
  if (nrow(all_continuous) > 0) {
    output$Continuous <- all_continuous %>%
      dplyr::group_by(Task, Variable) %>%
      dplyr::summarise(dplyr::across(
        c(MSE, RMSE, MSE_rel, MAE, R2_out),
        \(x) mean(x, na.rm = TRUE)
      ), .groups = 'drop') %>%
      dplyr::mutate(type = "Continuous")
  }
  
  if (nrow(all_categorical) > 0) {
    output$Categorical <- all_categorical %>%
      dplyr::group_by(Task, Variable) %>%
      dplyr::summarise(Accuracy = mean(Accuracy, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(type = "Binary")
  }
  
  # --- 2. Coefficient Accuracy ---
  if (!is.null(imp$models)) {
    coeff_summary <- evaluate.coefficients(imp$models, rawdata_std, where, show_warnings = show_warnings)
    if (!is.null(coeff_summary)) output$Coefficients <- coeff_summary
  }
  
  # --- 3. Return Combined Output ---
  if (length(output) > 0) return(output)
  
  stop("No data evaluated.")
}


#' Evaluate Coefficients from a MICE Analysis
#'
#' @description
#' Evaluates the coefficient estimation error from a `mice` analysis by comparing
#' the coefficients from a fitted model (`mira` object) against true values.
#'
#' @param fit A `mira` object, typically the result of running `with(mice_object, lm(...))`.
#' @param rawdata The original list of tasks. The element corresponding to the task
#'   should contain the true `B` and `covariates` matrices.
#' @param task_index An integer indicating which task from the `rawdata` list the
#'   `fit` object corresponds to.
#'
#' @return A data.frame with columns `Variable` (the response), and `Beta_MSE`.
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'tasks.mice' is a list of mids objects and 'rawdata' has true coefficients
#'
#' # 1. Fit an analysis model on the first imputed task
#' fit1 <- with(tasks.mice[[1]], lm(ic_x1 ~ c_x115 + c_x116 + c_x117))
#'
#' # 2. Evaluate the coefficients for this model
#' coeff_error <- evaluate.mice.coefficients(fit1, rawdata, task_index = 1)
#' print(coeff_error)
#' }
evaluate.mice.coefficients <- function(fit, rawdata, task_index) {
  if (!inherits(fit, "mira")) {
    stop("Error: 'fit' must be a 'mira' object from a `with()` call on a mids object.")
  }
  
  m <- length(fit$analyses)
  if (m == 0) {
    stop("Error: The 'fit' object contains no analyses.")
  }
  
  # --- Get information from the first model ---
  first_model <- fit$analyses[[1]]
  beta_hat_names <- names(coef(first_model))
  response_var_name <- all.vars(formula(first_model))[1]
  
  # --- Get true coefficient information from rawdata ---
  rawdata_task <- rawdata[[task_index]]
  if (is.null(rawdata_task[["B"]]) || is.null(rawdata_task[["covariates"]])) {
    warning("True coefficients 'B' or 'covariates' not found for this task.")
    return(NULL)
  }
  
  true_coeffs_matrix <- rawdata_task[["B"]]
  true_covariates_matrix <- rawdata_task[["covariates"]]
  
  if (!(response_var_name %in% colnames(true_covariates_matrix))) {
    warning(paste("No true covariates found for the response variable:", response_var_name))
    return(NULL)
  }
  
  col_idx <- match(response_var_name, colnames(true_covariates_matrix))
  true_predictor_names <- na.omit(true_covariates_matrix[, col_idx])
  true_predictor_coeffs <- na.omit(true_coeffs_matrix[, col_idx])
  
  named_true_coeffs <- setNames(true_predictor_coeffs, true_predictor_names)
  # mice lm fits the intercept by default, so we rename it to match coef()
  names(named_true_coeffs)[names(named_true_coeffs) == "intercept"] <- "(Intercept)"
  
  # --- Build the full true beta vector ---
  beta_true <- setNames(numeric(length(beta_hat_names)), beta_hat_names)
  valid_names <- intersect(names(beta_true), names(named_true_coeffs))
  
  if (length(valid_names) > 0) {
    beta_true[valid_names] <- named_true_coeffs[valid_names]
  }
  
  # --- Loop through models to calculate MSE ---
  mse_list <- numeric(m)
  for (h in 1:m) {
    beta_hat <- coef(fit$analyses[[h]])
    
    # Ensure alignment if model terms differ slightly (unlikely but safe)
    beta_hat_aligned <- setNames(numeric(length(beta_true)), names(beta_true))
    common_coefs <- intersect(names(beta_hat), names(beta_true))
    beta_hat_aligned[common_coefs] <- beta_hat[common_coefs]
    
    mse_list[h] <- mean((beta_hat_aligned - beta_true)^2)
  }
  
  # --- Summarize and return ---
  summary_df <- data.frame(
    Variable = response_var_name,
    Beta_MSE = mean(mse_list, na.rm = TRUE)
  )
  
  return(summary_df)
}


#' @rdname error.rate
error.rate.mice <- function(mice.imp, rawdata) {
  # This function remains focused on IMPUTATION accuracy, as the `mice.imp`
  # object does not contain model coefficients. Use `evaluate.mice.coefficients`
  # for coefficient evaluation after fitting a model.
  
  if (!is.list(rawdata)) {
    stop("Error: Input must be a list.")
  }
  rawdata <- lapply(rawdata, function(obj) {
    if (is.data.frame(obj)) {
      return(obj)
    } else if (is.matrix(obj)) {
      return(as.data.frame(obj))
    } else if(is.list(obj) && !is.null(obj[["data"]])){
      return(as.data.frame(obj[["data"]]))
    } else {
      stop("Error: Objects within the list must be either matrices or data frames.")
    }
  })
  
  # Assumes mice.imp is a list of mids objects, one per task
  if (!is.list(mice.imp) || !inherits(mice.imp[[1]], "mids")) {
    stop("'mice.imp' should be a list of mids objects.")
  }
  
  all_continuous <- list()
  all_categorical <- list()
  
  # Loop over each task (each mids object)
  for (i in seq_along(mice.imp)) {
    mids_obj <- mice.imp[[i]]
    rawdata_task <- rawdata[[i]]
    
    # Complete all imputations for this task
    completed_datasets <- mice::complete(mids_obj, "all")
    
    # Evaluate each imputed dataset
    for (j in seq_along(completed_datasets)) {
      eval_res <- evaluate.imputation(rawdata_task, completed_datasets[[j]], mids_obj$where)
      if (!is.null(eval_res)) {
        if (!is.null(eval_res$Continuous_Summary)) {
          res_cont <- eval_res$Continuous_Summary
          res_cont$Task <- i
          all_continuous <- append(all_continuous, list(res_cont))
        }
        if (!is.null(eval_res$Categorical_Summary)) {
          res_cat <- eval_res$Categorical_Summary
          res_cat$Task <- i
          all_categorical <- append(all_categorical, list(res_cat))
        }
      }
    }
  }
  
  output <- list()
  if (length(all_continuous) > 0) {
    continuous_summary <- dplyr::bind_rows(all_continuous) %>%
      dplyr::group_by(Task, Variable) %>%
      dplyr::summarise(dplyr::across(c(MSE, RMSE, MSE_rel, MAE, R2_out), \(x) mean(x, na.rm = TRUE)), .groups = 'drop') %>%
      dplyr::mutate(type = "Continuous")
    output$Continuous <- continuous_summary
  }
  if (length(all_categorical) > 0) {
    categorical_summary <- dplyr::bind_rows(all_categorical) %>%
      dplyr::group_by(Task, Variable) %>%
      dplyr::summarise(Accuracy = mean(Accuracy, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::mutate(type = "Binary")
    output$Categorical <- categorical_summary
  }
  
  if (length(output) > 0) {
    return(output)
  } else {
    stop("No imputation data evaluated.")
  }
}
