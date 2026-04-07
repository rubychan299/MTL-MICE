# Required pkgs
library(glmnet)
library(caret)
library(pROC)

# Helper: detect binary {0,1} (robust to factors/characters)
.is_binary <- function(y) {
  y0 <- y[!is.na(y)]
  u  <- unique(y0)
  length(u) == 2
}

# ---- Main screening function (saves coefficients) ----
lasso_screen_auto <- function(df_list, outcomes, 
                              r2_thresh = 0.6, auc_thresh = 0.75,
                              train_p = 0.7, seed = 123) {
  set.seed(seed)
  out <- vector("list", length(df_list))
  
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    predictors <- setdiff(names(df), outcomes)
    
    res_i <- vector("list", length(outcomes))
    names(res_i) <- outcomes
    
    for (yname in outcomes) {
      sub <- df[, c(yname, predictors), drop = FALSE]
      sub <- sub[stats::complete.cases(sub), , drop = FALSE]
      
      if (nrow(sub) < 20) {
        res_i[[yname]] <- list(metric = NA_character_, value = NA_real_,
                               selected = character(0), lambda = NA_real_,
                               coefs = setNames(rep(NA_real_, length(predictors) + 1),
                                                c("(Intercept)", predictors)))
        next
      }
      
      y_raw <- sub[[yname]]
      X     <- as.matrix(sub[, predictors, drop = FALSE])
      
      is_bin <- .is_binary(y_raw)
      
      # Split (stratify if binary)
      tr_idx <- tryCatch({
        if (is_bin) {
          createDataPartition(as.factor(y_raw), p = train_p, list = FALSE)
        } else {
          createDataPartition(y_raw, p = train_p, list = FALSE)
        }
      }, error = function(e) {
        sample.int(length(y_raw), size = floor(train_p * length(y_raw)))
      })
      
      x_tr <- X[tr_idx, , drop = FALSE]
      x_te <- X[-tr_idx, , drop = FALSE]
      y_tr_raw <- y_raw[tr_idx]
      y_te_raw <- y_raw[-tr_idx]
      
      # Default empty coef vector for this outcome (filled below on success)
      coef_names <- c("(Intercept)", predictors)
      empty_coefs <- setNames(rep(NA_real_, length(coef_names)), coef_names)
      
      if (is_bin) {
        # Map to {0,1}
        levs <- sort(unique(y_tr_raw))
        y_tr <- as.numeric(y_tr_raw == levs[2])
        y_te <- as.numeric(y_te_raw == levs[2])
        
        # Need both classes in training
        if (length(unique(y_tr)) < 2) {
          res_i[[yname]] <- list(metric = "AUC", value = NA_real_,
                                 selected = character(0), lambda = NA_real_,
                                 coefs = empty_coefs)
          next
        }
        
        cvfit <- cv.glmnet(
          x_tr, y_tr, family = "binomial", alpha = 1,
          standardize = TRUE, intercept = TRUE
        )
        
        # Probabilities and AUC
        p_te <- drop(predict(cvfit, newx = x_te, s = "lambda.min", type = "response"))
        roc_obj <- tryCatch(pROC::roc(response = y_te, predictor = p_te, quiet = TRUE),
                            error = function(e) NULL)
        auc_val <- if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_
        
        # Coefficients at lambda.min, aligned to (Intercept)+predictors
        beta <- as.matrix(coef(cvfit, s = "lambda.min"))
        bvec <- setNames(rep(0, length(coef_names)), coef_names)
        bvec[names(bvec) %in% rownames(beta)] <- beta[coef_names[coef_names %in% rownames(beta)], 1]
        sel  <- setdiff(names(bvec)[bvec != 0], "(Intercept)")
        
        res_i[[yname]] <- list(
          metric   = "AUC",
          value    = auc_val,
          selected = sel,
          lambda   = cvfit$lambda.min,
          coefs    = bvec
        )
        
      } else {
        # Continuous: gaussian + R^2
        y_tr <- as.numeric(y_tr_raw)
        y_te <- as.numeric(y_te_raw)
        
        if (length(unique(y_tr)) < 2 || stats::var(y_tr) == 0) {
          res_i[[yname]] <- list(metric = "R2", value = NA_real_,
                                 selected = character(0), lambda = NA_real_,
                                 coefs = empty_coefs)
          next
        }
        
        cvfit <- cv.glmnet(
          x_tr, y_tr, family = "gaussian", alpha = 1,
          standardize = TRUE, intercept = TRUE
        )
        yhat <- drop(predict(cvfit, newx = x_te, s = "lambda.min"))
        sse  <- sum((y_te - yhat)^2)
        sst  <- sum((y_te - mean(y_te))^2)
        r2   <- if (sst > 0) 1 - sse/sst else NA_real_
        
        # Coefficients at lambda.min, aligned to (Intercept)+predictors
        beta <- as.matrix(coef(cvfit, s = "lambda.min"))
        bvec <- setNames(rep(0, length(coef_names)), coef_names)
        bvec[names(bvec) %in% rownames(beta)] <- beta[coef_names[coef_names %in% rownames(beta)], 1]
        sel  <- setdiff(names(bvec)[bvec != 0], "(Intercept)")
        
        res_i[[yname]] <- list(
          metric   = "R2",
          value    = r2,
          selected = sel,
          lambda   = cvfit$lambda.min,
          coefs    = bvec
        )
      }
    }
    
    out[[i]] <- res_i
  }
  
  attr(out, "r2_thresh")  <- r2_thresh
  attr(out, "auc_thresh") <- auc_thresh
  out
}

# ---- Threshold filter (fixes small typo: entry$value, not entry.value) ----
filter_screen <- function(screen_result) {
  r2_thresh  <- attr(screen_result, "r2_thresh", exact = TRUE); if (is.null(r2_thresh)) r2_thresh <- 0.6
  auc_thresh <- attr(screen_result, "auc_thresh", exact = TRUE); if (is.null(auc_thresh)) auc_thresh <- 0.75
  
  lapply(screen_result, function(per_df) {
    lapply(per_df, function(entry) {
      if (is.null(entry$metric) || is.na(entry$metric) || is.na(entry$value)) return(character(0))
      ok <- (entry$metric == "R2"  && entry$value >= r2_thresh) ||
        (entry$metric == "AUC" && entry$value >= auc_thresh)
      if (ok) entry$selected else character(0)
    })
  })
}

# ---- Collect coefficients into aligned matrices ----
# Returns: a named list by outcome; each element is a data.frame:
# rows = data frame index (1..length(df_list)), cols = (Intercept)+predictors
collect_coefs <- function(screen_result) {
  # infer full set of coefficient names from first non-NA entry
  outcomes <- names(screen_result[[1]])
  out <- vector("list", length(outcomes)); names(out) <- outcomes
  
  for (yname in outcomes) {
    # gather rows across data frames
    rows <- lapply(screen_result, function(per_df) {
      co <- per_df[[yname]]$coefs
      if (is.null(co)) return(NULL)
      co
    })
    # align by names and bind
    all_names <- Reduce(union, lapply(rows, names))
    mat <- do.call(rbind, lapply(rows, function(v) {
      vv <- setNames(rep(NA_real_, length(all_names)), all_names)
      vv[names(v)] <- v
      vv
    }))
    rownames(mat) <- paste0("df", seq_len(nrow(mat)))
    out[[yname]] <- as.data.frame(mat, check.names = FALSE)
  }
  out
}

# ---- Summarize differences across data frames for each outcome ----
# For each outcome, compute per-variable min/max/range/sd and sparsity stats.
summarize_coef_diffs <- function(coef_tables) {
  lapply(coef_tables, function(tab) {
    # exclude NAs from stats; keep intercept in output but you can drop it if desired
    nz_count <- colSums(tab != 0 & !is.na(tab))
    n_obs    <- colSums(!is.na(tab))
    minv <- apply(tab, 2, function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE))
    maxv <- apply(tab, 2, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
    rng  <- maxv - minv
    sdev <- apply(tab, 2, function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE))
    
    data.frame(
      variable      = colnames(tab),
      n_nonzero     = as.integer(nz_count),
      n_models      = as.integer(n_obs),
      prop_nonzero  = ifelse(n_obs > 0, nz_count / n_obs, NA_real_),
      min           = minv,
      max           = maxv,
      range         = rng,
      sd            = sdev,
      row.names     = NULL,
      check.names   = FALSE
    )
  })
}

# ---- Optional: pairwise absolute-difference matrices per outcome ----
# Returns list(outcome -> 3D array [df x df x variable]) or a list of lists.
pairwise_absdiff <- function(coef_tables) {
  lapply(coef_tables, function(tab) {
    n <- nrow(tab); p <- ncol(tab)
    vars <- colnames(tab)
    res <- vector("list", p); names(res) <- vars
    for (j in seq_len(p)) {
      v <- tab[[j]]
      M <- matrix(NA_real_, n, n)
      for (a in seq_len(n)) for (b in seq_len(n)) {
        if (!is.na(v[a]) && !is.na(v[b])) M[a, b] <- abs(v[a] - v[b])
      }
      rownames(M) <- rownames(tab); colnames(M) <- rownames(tab)
      res[[j]] <- M
    }
    res
  })
}



kept <- lasso_screen_auto(uselect_target_list, outcomes = c("SBO015207","HSG495213","SBO315207","HSG445213", "HSG096213"))
coeff <- collect_coefs(kept)
coeff_summary <- summarize_coef_diffs(coeff)
coeff_diff <- pairwise_absdiff(coeff)

