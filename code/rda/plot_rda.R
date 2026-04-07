rm(list = ls())
library(readr)
library(foreach)
library(glmnet)
library(caret)
library(doParallel)
library(parallel)
library(tidyverse)

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
  mtlmice_pool = "Pool-MTL-MICE+",
  mtlmice      = "MTL-MICE+",
  mtlmice0     = "MTL-MICE",
  mice         = "Local-MICE",
  mice_pool   = "Pool-MICE"
)

iterations <- 100

# missing rate = 0.1####

file_names <- dir(path = "/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.1", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^uselect_imp_([0-9]+)\\.RData$", "\\1",file_names)))


mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)
mtlmice_id_list <- vector("list", iterations)

for(i in numbers){
  
  file_name <- paste0("/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.1/uselect_imp","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_0.1 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_0.1 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_0.1 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_0.1 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_0.1 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_0.1 <- summarize_mtlmice_pool(output_mtlmice_pool_0.1, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_0.1 <- summarize_mtlmice_pool(output_mtlmice_0.1, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_0.1 <- summarize_mtlmice_pool(output_mtlmice0_0.1, "mtlmice0", iterations = length(numbers))
output_mice_sum_0.1 <- summarize_mtlmice_pool(output_mice_0.1, "mice", iterations = length(numbers))
output_mice_pool_sum_0.1 <- summarize_mtlmice_pool(output_mice_pool_0.1, "mice_pool", iterations = length(numbers))


output_categorical_summary_0.1 <- rbind(output_mtlmice_pool_sum_0.1$Categorical, 
                                        output_mtlmice_sum_0.1$Categorical, 
                                        output_mtlmice0_sum_0.1$Categorical, 
                                        output_mice_sum_0.1$Categorical,
                                        output_mice_pool_sum_0.1$Categorical)

output_continuous_summary_0.1 <- rbind(output_mtlmice_pool_sum_0.1$Continuous,
                                       output_mtlmice_sum_0.1$Continuous, 
                                       output_mtlmice0_sum_0.1$Continuous, 
                                       output_mice_sum_0.1$Continuous,
                                       output_mice_pool_sum_0.1$Continuous)


output_categorical_mean_0.1 <- output_categorical_summary_0.1 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_0.1 <- output_continuous_summary_0.1 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

# missing rate = 0.2####

file_names <- dir(path = "/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.2", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^uselect_imp_([0-9]+)\\.RData$", "\\1",file_names)))


mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.2/uselect_imp","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_0.2 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_0.2 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_0.2 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_0.2 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_0.2 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_0.2 <- summarize_mtlmice_pool(output_mtlmice_pool_0.2, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_0.2 <- summarize_mtlmice_pool(output_mtlmice_0.2, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_0.2 <- summarize_mtlmice_pool(output_mtlmice0_0.2, "mtlmice0", iterations = length(numbers))
output_mice_sum_0.2 <- summarize_mtlmice_pool(output_mice_0.2, "mice", iterations = length(numbers))
output_mice_pool_sum_0.2 <- summarize_mtlmice_pool(output_mice_pool_0.2, "mice_pool", iterations = length(numbers))


output_categorical_summary_0.2 <- rbind(output_mtlmice_pool_sum_0.2$Categorical, 
                                        output_mtlmice_sum_0.2$Categorical, 
                                        output_mtlmice0_sum_0.2$Categorical, 
                                        output_mice_sum_0.2$Categorical,
                                        output_mice_pool_sum_0.2$Categorical)

output_continuous_summary_0.2 <- rbind(output_mtlmice_pool_sum_0.2$Continuous,
                                       output_mtlmice_sum_0.2$Continuous, 
                                       output_mtlmice0_sum_0.2$Continuous, 
                                       output_mice_sum_0.2$Continuous,
                                       output_mice_pool_sum_0.2$Continuous)


output_categorical_mean_0.2 <- output_categorical_summary_0.2 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_0.2 <- output_continuous_summary_0.2 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

# missing rate = 0.3####

file_names <- dir(path = "/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.3", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^uselect_imp_([0-9]+)\\.RData$", "\\1",file_names)))


mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.3/uselect_imp","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_0.3 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_0.3 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_0.3 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_0.3 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_0.3 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_0.3 <- summarize_mtlmice_pool(output_mtlmice_pool_0.3, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_0.3 <- summarize_mtlmice_pool(output_mtlmice_0.3, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_0.3 <- summarize_mtlmice_pool(output_mtlmice0_0.3, "mtlmice0", iterations = length(numbers))
output_mice_sum_0.3 <- summarize_mtlmice_pool(output_mice_0.3, "mice", iterations = length(numbers))
output_mice_pool_sum_0.3 <- summarize_mtlmice_pool(output_mice_pool_0.3, "mice_pool", iterations = length(numbers))


output_categorical_summary_0.3 <- rbind(output_mtlmice_pool_sum_0.3$Categorical, 
                                        output_mtlmice_sum_0.3$Categorical, 
                                        output_mtlmice0_sum_0.3$Categorical, 
                                        output_mice_sum_0.3$Categorical,
                                        output_mice_pool_sum_0.3$Categorical)

output_continuous_summary_0.3 <- rbind(output_mtlmice_pool_sum_0.3$Continuous,
                                       output_mtlmice_sum_0.3$Continuous, 
                                       output_mtlmice0_sum_0.3$Continuous, 
                                       output_mice_sum_0.3$Continuous,
                                       output_mice_pool_sum_0.3$Continuous)


output_categorical_mean_0.3 <- output_categorical_summary_0.3 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_0.3 <- output_continuous_summary_0.3 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

# missing rate = 0.4####

file_names <- dir(path = "/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.4", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^uselect_imp_([0-9]+)\\.RData$", "\\1",file_names)))


mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.4/uselect_imp","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_0.4 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_0.4 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_0.4 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_0.4 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_0.4 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_0.4 <- summarize_mtlmice_pool(output_mtlmice_pool_0.4, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_0.4 <- summarize_mtlmice_pool(output_mtlmice_0.4, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_0.4 <- summarize_mtlmice_pool(output_mtlmice0_0.4, "mtlmice0", iterations = length(numbers))
output_mice_sum_0.4 <- summarize_mtlmice_pool(output_mice_0.4, "mice", iterations = length(numbers))
output_mice_pool_sum_0.4 <- summarize_mtlmice_pool(output_mice_pool_0.4, "mice_pool", iterations = length(numbers))


output_categorical_summary_0.4 <- rbind(output_mtlmice_pool_sum_0.4$Categorical, 
                                        output_mtlmice_sum_0.4$Categorical, 
                                        output_mtlmice0_sum_0.4$Categorical, 
                                        output_mice_sum_0.4$Categorical,
                                        output_mice_pool_sum_0.4$Categorical)

output_continuous_summary_0.4 <- rbind(output_mtlmice_pool_sum_0.4$Continuous,
                                       output_mtlmice_sum_0.4$Continuous, 
                                       output_mtlmice0_sum_0.4$Continuous, 
                                       output_mice_sum_0.4$Continuous,
                                       output_mice_pool_sum_0.4$Continuous)


output_categorical_mean_0.4 <- output_categorical_summary_0.4 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_0.4 <- output_continuous_summary_0.4 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

# missing rate = 0.5####

file_names <- dir(path = "/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.5", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^uselect_imp_([0-9]+)\\.RData$", "\\1",file_names)))


mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/users/yuyuchen/library/cloudstorage/dropbox/new york university/research/feng lab/mtl-mice/rda/output_0.5/uselect_imp","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_0.5 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_0.5 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_0.5 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_0.5 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_0.5 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_0.5 <- summarize_mtlmice_pool(output_mtlmice_pool_0.5, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_0.5 <- summarize_mtlmice_pool(output_mtlmice_0.5, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_0.5 <- summarize_mtlmice_pool(output_mtlmice0_0.5, "mtlmice0", iterations = length(numbers))
output_mice_sum_0.5 <- summarize_mtlmice_pool(output_mice_0.5, "mice", iterations = length(numbers))
output_mice_pool_sum_0.5 <- summarize_mtlmice_pool(output_mice_pool_0.5, "mice_pool", iterations = length(numbers))


output_categorical_summary_0.5 <- rbind(output_mtlmice_pool_sum_0.5$Categorical, 
                                        output_mtlmice_sum_0.5$Categorical, 
                                        output_mtlmice0_sum_0.5$Categorical, 
                                        output_mice_sum_0.5$Categorical,
                                        output_mice_pool_sum_0.5$Categorical)

output_continuous_summary_0.5 <- rbind(output_mtlmice_pool_sum_0.5$Continuous,
                                       output_mtlmice_sum_0.5$Continuous, 
                                       output_mtlmice0_sum_0.5$Continuous, 
                                       output_mice_sum_0.5$Continuous,
                                       output_mice_pool_sum_0.5$Continuous)


output_categorical_mean_0.5 <- output_categorical_summary_0.5 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_0.5 <- output_continuous_summary_0.5 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )


# Plots - Missing percentage####


output_continuous_summary <- bind_rows(output_continuous_summary_0.1, 
                                       output_continuous_summary_0.2, 
                                       output_continuous_summary_0.3,
                                       output_continuous_summary_0.4,
                                       output_continuous_summary_0.5,
                                       .id = "missing_rate") %>%
  
  mutate(
    Task = case_when(
      Task == 1 ~ "AR",
      Task == 2 ~ "GA",
      Task == 3 ~ "IL",
      Task == 4 ~ "MI",
      Task == 5 ~ "MN",
      Task == 6 ~ "MS",
      Task == 7 ~ "NC",
      Task == 8 ~ "VA"
    )) %>% 
  dplyr::select(Task, method, missing_rate, Variable, mean_MSE_rel, mean_MAE, mean_R2_out,
                se_MSE_rel, se_MAE, se_R2_out)


output_continuous_summary$missing_rate <- rep(c(0.1, 0.2, 0.3, 0.4, 0.5), each = nrow(output_continuous_summary_0.2))


output_categorical_summary <- bind_rows(output_categorical_summary_0.1, 
                                        output_categorical_summary_0.2, 
                                        output_categorical_summary_0.3,
                                        output_categorical_summary_0.4,
                                        output_categorical_summary_0.5,
                                        .id = "missing_rate") %>% 
  mutate(
    Task = case_when(
      Task == 1 ~ "AR",
      Task == 2 ~ "GA",
      Task == 3 ~ "IL",
      Task == 4 ~ "MI",
      Task == 5 ~ "MN",
      Task == 6 ~ "MS",
      Task == 7 ~ "NC",
      Task == 8 ~ "VA"
    )) %>% 
  dplyr::select(Task, method, missing_rate, Variable, mean_Accuracy,
                se_Accuracy)

output_categorical_summary$missing_rate <- rep(c(0.1,0.2, 0.3, 0.4, 0.5), each = nrow(output_categorical_summary_0.2))

# write.csv(output_continuous_summary, "../rda_nhanes_byyear_continuous_summary.csv", row.names = FALSE)
# write.csv(output_categorical_summary, "../rda_nhanes_byyear_categorical_summary.csv", row.names = FALSE)

output_continuous <- bind_rows(output_continuous_mean_0.1, 
                               output_continuous_mean_0.2, 
                               output_continuous_mean_0.3,
                               output_continuous_mean_0.4,
                               output_continuous_mean_0.5,
                               .id = "missing_rate") %>% 
  mutate(
  Task = case_when(
    Task == 1 ~ "AR",
    Task == 2 ~ "GA",
    Task == 3 ~ "IL",
    Task == 4 ~ "MI",
    Task == 5 ~ "MN",
    Task == 6 ~ "MS",
    Task == 7 ~ "NC",
    Task == 8 ~ "VA"
  ))
output_continuous$missing_rate <- rep(c(0.1, 0.2, 0.3, 0.4, 0.5), each = nrow(output_continuous_mean_0.2))

output_categorical <- bind_rows(output_categorical_mean_0.1, 
                                output_categorical_mean_0.2, 
                                output_categorical_mean_0.3,
                                output_categorical_mean_0.4,
                                output_categorical_mean_0.5,
                                .id = "missing_rate") %>% 
  mutate(
  Task = case_when(
    Task == 1 ~ "AR",
    Task == 2 ~ "GA",
    Task == 3 ~ "IL",
    Task == 4 ~ "MI",
    Task == 5 ~ "MN",
    Task == 6 ~ "MS",
    Task == 7 ~ "NC",
    Task == 8 ~ "VA"
  ))
output_categorical$missing_rate <- rep(c(0.1, 0.2, 0.3, 0.4, 0.5), each = nrow(output_categorical_mean_0.2))


p.continuous <- output_continuous %>% 
  group_by(Task, missing_rate, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"),   mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = missing_rate, y = mean_mean_MSE_rel, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(ymin = mean_mean_MSE_rel - mean_se_MSE_rel,
        ymax = mean_mean_MSE_rel + mean_se_MSE_rel),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "Missing Rate", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  facet_wrap(~ Task, ncol = 4)

ggsave(p.continuous, file = "./results/rda_uselection_continuous_bystate_plot.pdf", width = 10, height = 8, units = "in", dpi = 300)


p.continuous <- output_continuous %>% 
  group_by(missing_rate, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"),   mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = missing_rate, y = mean_mean_MSE_rel, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(ymin = mean_mean_MSE_rel - mean_se_MSE_rel,
        ymax = mean_mean_MSE_rel + mean_se_MSE_rel),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "Missing Rate", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p.continuous, file = "./results/rda_uselection_continuous_plot.pdf", width = 10, height = 8, units = "in", dpi = 300)

p.continuous.rss <- output_continuous %>% 
  group_by(Task, missing_rate, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"),   mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = missing_rate, y = 1-mean_mean_R2_out, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(ymin = 1-mean_mean_R2_out - mean_se_R2_out,
        ymax = 1-mean_mean_R2_out + mean_se_R2_out),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "Missing Rate", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  facet_wrap(~ Task, ncol = 4)

ggsave(p.continuous.rss, file = "./results/rda_uselection_continuous_rss_bystate_plot.pdf", width = 10, height = 8, units = "in", dpi = 300)


p.continuous.rss <- output_continuous %>% 
  group_by(missing_rate, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"),   mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = missing_rate, y = 1-mean_mean_R2_out, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(ymin = 1-mean_mean_R2_out - mean_se_R2_out,
        ymax = 1-mean_mean_R2_out + mean_se_R2_out),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "Missing Rate", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(p.continuous.rss, file = "./results/rda_uselection_continuous_rss_plot.pdf", width = 10, height = 8, units = "in", dpi = 300)

p.categorical <- output_categorical %>% 
  ggplot(aes(x = missing_rate, y = mean_mean_Accuracy, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Accuracy - mean_se_Accuracy,
      ymax = mean_mean_Accuracy + mean_se_Accuracy
    ),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "Missing Rate", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  facet_wrap(~Task, ncol = 4)

ggsave(p.categorical, file = "./results/rda_uselection_categorical_bystate_plot.pdf", width = 10, height = 8, units = "in", dpi = 300)


p.categorical <- output_categorical %>% 
  group_by(missing_rate, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = missing_rate, y = mean_mean_Accuracy, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Accuracy - mean_se_Accuracy,
      ymax = mean_mean_Accuracy + mean_se_Accuracy
    ),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "Missing Rate", y = "Accuracy") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.categorical, file = "./results/rda_uselection_categorical_plot.pdf", width = 10, height = 8, units = "in", dpi = 300)


p.combined <- patchwork::wrap_plots(
  p.continuous.rss,
  p.categorical,
  ncol = 2,
  guides = "collect"
) +
  patchwork::plot_annotation(
    tag_levels = "a",   # a, b, c, d
    tag_suffix = ")"    # a), b), c), d)
  ) &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 13),
    plot.tag.position = c(0.02, 1.02),   # top-left corner
    plot.tag.location = "panel",
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5)
  )

ggsave(
  p.combined,
  file = "./results/rda_uselection_combined_plot.pdf",
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

