rm(list = ls())
library(readr)
library(foreach)
library(glmnet)
library(caret)
library(doParallel)
library(parallel)
library(tidyverse)
library(igraph)

scripts.sources <-  list.files("./R", 
                               pattern="*.R$", full.names=TRUE, 
                               ignore.case=TRUE)
sapply(scripts.sources,source,.GlobalEnv)

load("./data/uselection/bystate_2020/uselect_list2020.RData")

method_colors <- c(
  mtlmice_pool = "#1F77B4",  # strong blue
  mtlmice      = "#FF7F0E",  # strong orange
  mtlmice0     = "#9467BD",  # purple
  mice         = "#2CA02C",  # green
  mice_pool   = "#D62728"   # red
)

method_labels <- c(
  mtlmice_pool = "Pool-MTL-MICE",
  mtlmice      = "MTL-MICE+",
  mtlmice0     = "MTL-MICE",
  mice         = "Local-MICE",
  mice_pool   = "Pool-MICE"
)

iterations <- 100

# missing rate = 0.1####

file_names <- dir(path = "/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.1", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^uselect_imp_([0-9]+)\\.RData$", "\\1",file_names)))


mtlmice_id_list <- vector("list", iterations)

extract_transfer_ids <- function(mtlmice_obj) {
  task_names <- names(mtlmice_obj$data)
  if (is.null(task_names) || any(task_names == "")) {
    task_names <- as.character(seq_along(mtlmice_obj$data))
  }
  models <- mtlmice_obj$models
  lapply(seq_along(models), function(m_idx) {
    per_task <- models[[m_idx]]
    out_task <- lapply(seq_along(per_task), function(task_idx) {
      per_var <- per_task[[task_idx]]
      var_names <- colnames(mtlmice_obj$data[[task_idx]])
      if (is.null(var_names) || any(var_names == "")) {
        var_names <- as.character(seq_along(per_var))
      }
      out_var <- lapply(per_var, function(model) {
        if (is.null(model)) return(NULL)
        transfer_id <- model$transfer.id
        if (is.null(transfer_id)) return(NULL)
        if (length(transfer_id) == 1 && transfer_id == 0) return(character(0))
        source_indices <- setdiff(seq_along(task_names), task_idx)
        mapped <- source_indices[transfer_id]
        if (any(is.na(mapped))) {
          return(transfer_id)
        }
        task_names[mapped]
      })
      names(out_var) <- var_names
      out_var
    })
    names(out_task) <- task_names
    out_task
  })
}

for(i in numbers){
  
  file_name <- paste0("/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.1/uselect_imp","_", i, ".RData")
  load(file_name)
  
  mtlmice_id_list[[i]] <- extract_transfer_ids(uselect_mtlmice)
  
}

build_transfer_long <- function(mtlmice_id_list, iteration_ids = NULL) {
  non_null_idx <- which(!vapply(mtlmice_id_list, is.null, logical(1)))
  if (length(non_null_idx) == 0) return(tibble())
  if (is.null(iteration_ids)) {
    iter_ids <- non_null_idx
  } else if (length(iteration_ids) == length(mtlmice_id_list)) {
    iter_ids <- iteration_ids[non_null_idx]
  } else if (length(iteration_ids) == length(non_null_idx)) {
    iter_ids <- iteration_ids
  } else {
    name_ids <- names(mtlmice_id_list)[non_null_idx]
    if (!is.null(name_ids) && all(nzchar(name_ids))) {
      iter_ids <- name_ids
    } else {
      iter_ids <- non_null_idx
    }
  }
  purrr::map2_dfr(mtlmice_id_list[non_null_idx], iter_ids, function(m_list, iter_id) {
    if (is.null(m_list)) return(tibble())
    purrr::imap_dfr(m_list, function(task_list, m_idx) {
      purrr::imap_dfr(task_list, function(var_list, target_task) {
        purrr::imap_dfr(var_list, function(selected_sources, var_name) {
          if (is.null(selected_sources) || length(selected_sources) == 0) return(tibble())
          tibble(
            iteration = iter_id,
            imputation = m_idx,
            target_task = target_task,
            variable = var_name,
            source_task = selected_sources
          )
        })
      })
    })
  })
}

build_model_index <- function(mtlmice_id_list, iteration_ids = NULL) {
  non_null_idx <- which(!vapply(mtlmice_id_list, is.null, logical(1)))
  if (length(non_null_idx) == 0) return(tibble())
  if (is.null(iteration_ids)) {
    iter_ids <- non_null_idx
  } else if (length(iteration_ids) == length(mtlmice_id_list)) {
    iter_ids <- iteration_ids[non_null_idx]
  } else if (length(iteration_ids) == length(non_null_idx)) {
    iter_ids <- iteration_ids
  } else {
    name_ids <- names(mtlmice_id_list)[non_null_idx]
    if (!is.null(name_ids) && all(nzchar(name_ids))) {
      iter_ids <- name_ids
    } else {
      iter_ids <- non_null_idx
    }
  }
  purrr::map2_dfr(mtlmice_id_list[non_null_idx], iter_ids, function(m_list, iter_id) {
    if (is.null(m_list)) return(tibble())
    purrr::imap_dfr(m_list, function(task_list, m_idx) {
      purrr::imap_dfr(task_list, function(var_list, target_task) {
        purrr::imap_dfr(var_list, function(selected_sources, var_name) {
          if (is.null(selected_sources)) return(tibble())
          tibble(
            iteration = iter_id,
            imputation = m_idx,
            target_task = target_task,
            variable = var_name
          )
        })
      })
    })
  })
}

transfer_long <- build_transfer_long(mtlmice_id_list, iteration_ids = numbers)
transfer_models <- build_model_index(mtlmice_id_list, iteration_ids = numbers)

transfer_summary <- transfer_long %>%
  count(target_task, variable, source_task, name = "n_selected") %>%
  left_join(
    transfer_models %>% count(target_task, variable, name = "n_models"),
    by = c("target_task", "variable")
  ) %>%
  mutate(freq = n_selected / n_models) %>%
  arrange(target_task, variable, desc(freq), source_task) %>% 
  mutate(
    target_task_num = suppressWarnings(as.integer(target_task)),
    variable_num = suppressWarnings(as.integer(variable)),
    target_task = case_when(
      !is.na(target_task_num) & target_task_num == 1 ~ "AR",
      !is.na(target_task_num) & target_task_num == 2 ~ "GA",
      !is.na(target_task_num) & target_task_num == 3 ~ "IL",
      !is.na(target_task_num) & target_task_num == 4 ~ "MI",
      !is.na(target_task_num) & target_task_num == 5 ~ "MN",
      !is.na(target_task_num) & target_task_num == 6 ~ "MS",
      !is.na(target_task_num) & target_task_num == 7 ~ "NC",
      !is.na(target_task_num) & target_task_num == 8 ~ "VA",
      TRUE ~ as.character(target_task)
    ),
    variable = case_when(
      !is.na(variable_num) & variable_num == 27 ~ "Median Housing Units",
      !is.na(variable_num) & variable_num == 52 ~ "Multi-Unit Housing >= 10%",
      TRUE ~ as.character(variable)
    )
  ) %>%
  dplyr::select(-target_task_num, -variable_num)

pair_summary <- transfer_summary %>%
  filter(!is.na(source_task), !is.na(target_task)) %>%
  group_by(source_task, target_task, variable) %>%
  summarise(
    n_selected = sum(n_selected, na.rm = TRUE),
    n_models = sum(n_models, na.rm = TRUE),
    freq = n_selected / n_models,
    .groups = "drop"
  ) %>%
  filter(is.finite(freq), source_task != target_task)

top_n_pairs <- 10
top_transfer_pairs <- pair_summary %>%
  group_by(variable, target_task) %>%
  arrange(desc(freq)) %>%
  slice_head(n = top_n_pairs)

library(dplyr)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(grid)

plot_transfer_network <- function(df,
                                  weight_col = "freq",
                                  agg = c("max", "mean"),
                                  seed = 7,
                                  layout = "fr",
                                  curvature = 0.12,
                                  scale_edge_widths = FALSE,
                                  node_size = 14,          # <-- control node size here
                                  label_size = 5,
                                  arrow_mm = 6,
                                  savepath = NULL,
                                  width = 16,
                                  height = 12,
                                  dpi = 300) {
  agg <- match.arg(agg)
  d <- df

  if (!weight_col %in% names(d)) {
    if (all(c("n_selected", "n_models") %in% names(d))) {
      d <- d %>% mutate(freq = .data$n_selected / .data$n_models)
      weight_col <- "freq"
    } else {
      stop("Missing 'freq' (or weight_col) and cannot compute it (need n_selected and n_models).")
    }
  }

  d_pair <- d %>%
    group_by(.data$source_task, .data$target_task) %>%
    summarise(
      w = if (agg == "max") max(.data[[weight_col]], na.rm = TRUE) else mean(.data[[weight_col]], na.rm = TRUE),
      n_selected = if ("n_selected" %in% names(d)) max(.data$n_selected, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  if (all(!is.na(d_pair$n_selected))) {
    d_top <- d_pair %>% arrange(desc(.data$w), desc(.data$n_selected)) 
  } else {
    d_top <- d_pair %>% arrange(desc(.data$w))
  }

  targets <- unique(d_top$target_task)

  g <- graph_from_data_frame(
    d = d_top %>% transmute(from = .data$source_task, to = .data$target_task, w = .data$w),
    directed = TRUE
  )

  tg <- as_tbl_graph(g) %>%
    activate(nodes) %>%
    mutate(
      is_target = name %in% targets,
      node_group = factor(ifelse(is_target, "Targets", "Other states"),
                          levels = c("Targets", "Other states"))
    ) %>%
    activate(edges) %>%
    mutate(
      loop = tidygraph::edge_is_loop(),
      w_plot = if (scale_edge_widths) w else 1
    )

  set.seed(seed)

  # Caps must be large enough so arrowheads are not under the node
  cap_mm <- node_size * 0.4 # tune if you change node_size a lot

  p <- ggraph(tg, layout = layout) +
    geom_edge_arc(
      aes(edge_width = w_plot, filter = !loop),
      edge_colour = "grey35",
      alpha = 0.9,
      strength = curvature,
      arrow = arrow(length = unit(arrow_mm, "mm"), type = "closed"),
      end_cap = circle(cap_mm, "mm"),
      start_cap = circle(cap_mm, "mm"),
      lineend = "round"
    ) +
    geom_edge_loop(
      aes(edge_width = w_plot, filter = loop),
      edge_colour = "grey35",
      alpha = 0.9,
      arrow = arrow(length = unit(arrow_mm, "mm"), type = "closed"),
      end_cap = circle(cap_mm, "mm"),
      start_cap = circle(cap_mm, "mm"),
      lineend = "round"
    ) +
    geom_node_point(
      aes(fill = node_group),
      size = node_size,
      shape = 21,
      colour = "black",
      stroke = 1.2
    ) +
    geom_node_text(
      aes(label = name),
      family = "serif",
      fontface = "bold",
      colour = "navy",
      size = label_size
    ) +
    scale_fill_manual(
      values = c("Targets" = "#F5A000", "Other states" = "#A7D7E8"),
      name = NULL
    ) +
    { if (scale_edge_widths)
        scale_edge_width(range = c(0.6, 2.2), guide = "none")
      else
        scale_edge_width(range = c(0.9, 0.9), guide = "none")
    } +
    theme_void() +
    theme(legend.position = "bottom")

  if (!is.null(savepath)) {
    dir.create(dirname(savepath), recursive = TRUE, showWarnings = FALSE)
    # If your PDF viewer still drops arrowheads, try: device = cairo_pdf
    ggsave(savepath, p, width = width, height = height, dpi = dpi, bg = "white")
  }

  print(p)
  invisible(d_top)
}

plot_transfer_network(
  top_transfer_pairs |> filter(variable == "Median Housing Units"),
  node_size = 20,      # big nodes -> big caps
  arrow_mm = 2,
  savepath = "./results/rda_transfer_top10_con.pdf"
)

plot_transfer_network(
  top_transfer_pairs |> filter(variable == "Multi-Unit Housing >= 10%"),
  node_size = 20,      # big nodes -> big caps
  arrow_mm = 2,
  savepath = "./results/rda_transfer_top10_cat.pdf"
)
