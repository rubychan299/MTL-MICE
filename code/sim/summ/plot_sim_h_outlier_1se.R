rm(list = ls())
library(ggplot2)
library(tidyverse)
library(patchwork)


scripts.sources <-  list.files("./R", 
                               pattern="*.R$", full.names=TRUE, 
                               ignore.case=TRUE)
sapply(scripts.sources,source,.GlobalEnv)

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

# h trend####
iterations <- 100

p <- 150

iterations <- 100

## h = 0####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h0_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h0","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h0 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h0 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h0 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h0 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h0 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h0 <- summarize_mtlmice_pool(output_mtlmice_pool_h0, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h0 <- summarize_mtlmice_pool(output_mtlmice_h0, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h0 <- summarize_mtlmice_pool(output_mtlmice0_h0, "mtlmice0", iterations = length(numbers))
output_mice_sum_h0 <- summarize_mtlmice_pool(output_mice_h0, "mice", iterations = length(numbers))
output_mice_pool_sum_h0 <- summarize_mtlmice_pool(output_mice_pool_h0, "mice_pool", iterations = length(numbers))

output_continuous_summary_h0 <- rbind(output_mtlmice_pool_sum_h0$Continuous,
                                       output_mtlmice_sum_h0$Continuous, 
                                       output_mtlmice0_sum_h0$Continuous, 
                                       output_mice_sum_h0$Continuous,
                                       output_mice_pool_sum_h0$Continuous)

output_coeff_summary_h0 <- rbind(output_mtlmice_pool_sum_h0$Coefficients,
                                       output_mtlmice_sum_h0$Coefficients, 
                                       output_mtlmice0_sum_h0$Coefficients, 
                                       output_mice_sum_h0$Coefficients,
                                       output_mice_pool_sum_h0$Coefficients)

output_categorical_summary_h0 <- rbind(output_mtlmice_pool_sum_h0$Categorical, 
                                        output_mtlmice_sum_h0$Categorical, 
                                        output_mtlmice0_sum_h0$Categorical, 
                                        output_mice_sum_h0$Categorical,
                                        output_mice_pool_sum_h0$Categorical)

output_categorical_mean_h0 <- output_categorical_summary_h0 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_h0 <- output_continuous_summary_h0 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h0 <- output_coeff_summary_h0 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h0 <- output_coeff_summary_h0 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h0 <- output_coeff_summary_h0 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## h = 0.25####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h0\\.25_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h0.25","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h0.25 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h0.25 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h0.25 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h0.25 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h0.25 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h0.25 <- summarize_mtlmice_pool(output_mtlmice_pool_h0.25, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h0.25 <- summarize_mtlmice_pool(output_mtlmice_h0.25, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h0.25 <- summarize_mtlmice_pool(output_mtlmice0_h0.25, "mtlmice0", iterations = length(numbers))
output_mice_sum_h0.25 <- summarize_mtlmice_pool(output_mice_h0.25, "mice", iterations = length(numbers))
output_mice_pool_sum_h0.25 <- summarize_mtlmice_pool(output_mice_pool_h0.25, "mice_pool", iterations = length(numbers))


output_continuous_summary_h0.25 <- rbind(output_mtlmice_pool_sum_h0.25$Continuous,
                                       output_mtlmice_sum_h0.25$Continuous, 
                                       output_mtlmice0_sum_h0.25$Continuous, 
                                       output_mice_sum_h0.25$Continuous,
                                       output_mice_pool_sum_h0.25$Continuous)

output_coeff_summary_h0.25 <- rbind(output_mtlmice_pool_sum_h0.25$Coefficients,
                                       output_mtlmice_sum_h0.25$Coefficients, 
                                       output_mtlmice0_sum_h0.25$Coefficients, 
                                       output_mice_sum_h0.25$Coefficients,
                                       output_mice_pool_sum_h0.25$Coefficients)

output_categorical_summary_h0.25 <- rbind(output_mtlmice_pool_sum_h0.25$Categorical, 
                                        output_mtlmice_sum_h0.25$Categorical, 
                                        output_mtlmice0_sum_h0.25$Categorical, 
                                        output_mice_sum_h0.25$Categorical,
                                        output_mice_pool_sum_h0.25$Categorical)

output_categorical_mean_h0.25 <- output_categorical_summary_h0.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_h0.25 <- output_continuous_summary_h0.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h0.25 <- output_coeff_summary_h0.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h0.25 <- output_coeff_summary_h0.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h0.25 <- output_coeff_summary_h0.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## h = 0.5####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h0\\.5_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h0.5","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h0.5 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h0.5 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h0.5 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h0.5 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h0.5 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h0.5 <- summarize_mtlmice_pool(output_mtlmice_pool_h0.5, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h0.5 <- summarize_mtlmice_pool(output_mtlmice_h0.5, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h0.5 <- summarize_mtlmice_pool(output_mtlmice0_h0.5, "mtlmice0", iterations = length(numbers))
output_mice_sum_h0.5 <- summarize_mtlmice_pool(output_mice_h0.5, "mice", iterations = length(numbers))
output_mice_pool_sum_h0.5 <- summarize_mtlmice_pool(output_mice_pool_h0.5, "mice_pool", iterations = length(numbers))


output_continuous_summary_h0.5 <- rbind(output_mtlmice_pool_sum_h0.5$Continuous,
                                       output_mtlmice_sum_h0.5$Continuous, 
                                       output_mtlmice0_sum_h0.5$Continuous, 
                                       output_mice_sum_h0.5$Continuous,
                                       output_mice_pool_sum_h0.5$Continuous)

output_coeff_summary_h0.5 <- rbind(output_mtlmice_pool_sum_h0.5$Coefficients,
                                       output_mtlmice_sum_h0.5$Coefficients, 
                                       output_mtlmice0_sum_h0.5$Coefficients, 
                                       output_mice_sum_h0.5$Coefficients,
                                       output_mice_pool_sum_h0.5$Coefficients)

output_categorical_summary_h0.5 <- rbind(output_mtlmice_pool_sum_h0.5$Categorical, 
                                        output_mtlmice_sum_h0.5$Categorical, 
                                        output_mtlmice0_sum_h0.5$Categorical, 
                                        output_mice_sum_h0.5$Categorical,
                                        output_mice_pool_sum_h0.5$Categorical)

output_categorical_mean_h0.5 <- output_categorical_summary_h0.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 


output_continuous_mean_h0.5 <- output_continuous_summary_h0.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h0.5 <- output_coeff_summary_h0.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h0.5 <- output_coeff_summary_h0.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h0.5 <- output_coeff_summary_h0.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## h = 0.75####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h0\\.75_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h0.75","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h0.75 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h0.75 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h0.75 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h0.75 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h0.75 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h0.75 <- summarize_mtlmice_pool(output_mtlmice_pool_h0.75, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h0.75 <- summarize_mtlmice_pool(output_mtlmice_h0.75, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h0.75 <- summarize_mtlmice_pool(output_mtlmice0_h0.75, "mtlmice0", iterations = length(numbers))
output_mice_sum_h0.75 <- summarize_mtlmice_pool(output_mice_h0.75, "mice", iterations = length(numbers))
output_mice_pool_sum_h0.75 <- summarize_mtlmice_pool(output_mice_pool_h0.75, "mice_pool", iterations = length(numbers))


output_continuous_summary_h0.75 <- rbind(output_mtlmice_pool_sum_h0.75$Continuous,
                                       output_mtlmice_sum_h0.75$Continuous, 
                                       output_mtlmice0_sum_h0.75$Continuous, 
                                       output_mice_sum_h0.75$Continuous,
                                       output_mice_pool_sum_h0.75$Continuous)

output_coeff_summary_h0.75 <- rbind(output_mtlmice_pool_sum_h0.75$Coefficients,
                                       output_mtlmice_sum_h0.75$Coefficients, 
                                       output_mtlmice0_sum_h0.75$Coefficients, 
                                       output_mice_sum_h0.75$Coefficients,
                                       output_mice_pool_sum_h0.75$Coefficients)

output_categorical_summary_h0.75 <- rbind(output_mtlmice_pool_sum_h0.75$Categorical, 
                                        output_mtlmice_sum_h0.75$Categorical, 
                                        output_mtlmice0_sum_h0.75$Categorical, 
                                        output_mice_sum_h0.75$Categorical,
                                        output_mice_pool_sum_h0.75$Categorical)

output_categorical_mean_h0.75 <- output_categorical_summary_h0.75 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 

output_continuous_mean_h0.75 <- output_continuous_summary_h0.75 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h0.75 <- output_coeff_summary_h0.75 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h0.75 <- output_coeff_summary_h0.75 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h0.75 <- output_coeff_summary_h0.75 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## h = 1####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h1_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h1","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h1 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h1 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h1 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h1 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h1 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h1 <- summarize_mtlmice_pool(output_mtlmice_pool_h1, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h1 <- summarize_mtlmice_pool(output_mtlmice_h1, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h1 <- summarize_mtlmice_pool(output_mtlmice0_h1, "mtlmice0", iterations = length(numbers))
output_mice_sum_h1 <- summarize_mtlmice_pool(output_mice_h1, "mice", iterations = length(numbers))
output_mice_pool_sum_h1 <- summarize_mtlmice_pool(output_mice_pool_h1, "mice_pool", iterations = length(numbers))


output_continuous_summary_h1 <- rbind(output_mtlmice_pool_sum_h1$Continuous,
                                       output_mtlmice_sum_h1$Continuous, 
                                       output_mtlmice0_sum_h1$Continuous, 
                                       output_mice_sum_h1$Continuous,
                                       output_mice_pool_sum_h1$Continuous)

output_coeff_summary_h1 <- rbind(output_mtlmice_pool_sum_h1$Coefficients,
                                       output_mtlmice_sum_h1$Coefficients, 
                                       output_mtlmice0_sum_h1$Coefficients, 
                                       output_mice_sum_h1$Coefficients,
                                       output_mice_pool_sum_h1$Coefficients)

output_categorical_summary_h1 <- rbind(output_mtlmice_pool_sum_h1$Categorical, 
                                        output_mtlmice_sum_h1$Categorical, 
                                        output_mtlmice0_sum_h1$Categorical, 
                                        output_mice_sum_h1$Categorical,
                                        output_mice_pool_sum_h1$Categorical)

output_categorical_mean_h1 <- output_categorical_summary_h1 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 


output_continuous_mean_h1 <- output_continuous_summary_h1 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h1 <- output_coeff_summary_h1 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h1 <- output_coeff_summary_h1 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h1 <- output_coeff_summary_h1 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## h = 1.25####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h1\\.25_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h1.25","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h1.25 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h1.25 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h1.25 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h1.25 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h1.25 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h1.25 <- summarize_mtlmice_pool(output_mtlmice_pool_h1.25, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h1.25 <- summarize_mtlmice_pool(output_mtlmice_h1.25, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h1.25 <- summarize_mtlmice_pool(output_mtlmice0_h1.25, "mtlmice0", iterations = length(numbers))
output_mice_sum_h1.25 <- summarize_mtlmice_pool(output_mice_h1.25, "mice", iterations = length(numbers))
output_mice_pool_sum_h1.25 <- summarize_mtlmice_pool(output_mice_pool_h1.25, "mice_pool", iterations = length(numbers))


output_continuous_summary_h1.25 <- rbind(output_mtlmice_pool_sum_h1.25$Continuous,
                                       output_mtlmice_sum_h1.25$Continuous, 
                                       output_mtlmice0_sum_h1.25$Continuous, 
                                       output_mice_sum_h1.25$Continuous,
                                       output_mice_pool_sum_h1.25$Continuous)

output_coeff_summary_h1.25 <- rbind(output_mtlmice_pool_sum_h1.25$Coefficients,
                                       output_mtlmice_sum_h1.25$Coefficients, 
                                       output_mtlmice0_sum_h1.25$Coefficients, 
                                       output_mice_sum_h1.25$Coefficients,
                                       output_mice_pool_sum_h1.25$Coefficients)

output_categorical_summary_h1.25 <- rbind(output_mtlmice_pool_sum_h1.25$Categorical, 
                                        output_mtlmice_sum_h1.25$Categorical, 
                                        output_mtlmice0_sum_h1.25$Categorical, 
                                        output_mice_sum_h1.25$Categorical,
                                        output_mice_pool_sum_h1.25$Categorical)

output_categorical_mean_h1.25 <- output_categorical_summary_h1.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 


output_continuous_mean_h1.25 <- output_continuous_summary_h1.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h1.25 <- output_coeff_summary_h1.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h1.25 <- output_coeff_summary_h1.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h1.25 <- output_coeff_summary_h1.25 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## h = 1.5####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^h_outlier_1se_h1\\.5_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h1.5","_", i, ".RData")
  load(file_name) 

  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_h1.5 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .)  
output_mtlmice_h1.5 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_h1.5 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_h1.5 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_h1.5 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_h1.5 <- summarize_mtlmice_pool(output_mtlmice_pool_h1.5, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_h1.5 <- summarize_mtlmice_pool(output_mtlmice_h1.5, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_h1.5 <- summarize_mtlmice_pool(output_mtlmice0_h1.5, "mtlmice0", iterations = length(numbers))
output_mice_sum_h1.5 <- summarize_mtlmice_pool(output_mice_h1.5, "mice", iterations = length(numbers))
output_mice_pool_sum_h1.5 <- summarize_mtlmice_pool(output_mice_pool_h1.5, "mice_pool", iterations = length(numbers))


output_continuous_summary_h1.5 <- rbind(output_mtlmice_pool_sum_h1.5$Continuous,
                                       output_mtlmice_sum_h1.5$Continuous, 
                                       output_mtlmice0_sum_h1.5$Continuous, 
                                       output_mice_sum_h1.5$Continuous,
                                       output_mice_pool_sum_h1.5$Continuous)

output_coeff_summary_h1.5 <- rbind(output_mtlmice_pool_sum_h1.5$Coefficients,
                                       output_mtlmice_sum_h1.5$Coefficients, 
                                       output_mtlmice0_sum_h1.5$Coefficients, 
                                       output_mice_sum_h1.5$Coefficients,
                                       output_mice_pool_sum_h1.5$Coefficients)

output_categorical_summary_h1.5 <- rbind(output_mtlmice_pool_sum_h1.5$Categorical, 
                                        output_mtlmice_sum_h1.5$Categorical, 
                                        output_mtlmice0_sum_h1.5$Categorical, 
                                        output_mice_sum_h1.5$Categorical,
                                        output_mice_pool_sum_h1.5$Categorical)

output_categorical_mean_h1.5 <- output_categorical_summary_h1.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  ) 


output_continuous_mean_h1.5 <- output_continuous_summary_h1.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_h1.5 <- output_coeff_summary_h1.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_con_h1.5 <- output_coeff_summary_h1.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x1" | Variable == "ic_x2" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_cat_h1.5 <- output_coeff_summary_h1.5 %>% 
  group_by(Task, method, type_group) %>%
  filter(Variable == "ic_x3" | Variable == "ic_x4" ) %>%
  filter(Task <= 9) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## Plot ####

h <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)

output_continuous_h <- bind_rows(output_continuous_mean_h0, 
                               output_continuous_mean_h0.25,
                               output_continuous_mean_h0.5,
                               output_continuous_mean_h0.75,
                               output_continuous_mean_h1,
                               output_continuous_mean_h1.25,
                               output_continuous_mean_h1.5,
                               .id = "h")
output_continuous_h$h <- rep(h, each = nrow(output_continuous_mean_h0))

p.continuous <- output_continuous_h %>% 
  group_by(h, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = h, y = mean_mean_MSE_rel, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_MSE_rel - mean_se_MSE_rel,
      ymax = mean_mean_MSE_rel + mean_se_MSE_rel
    ),
    width = 0.01,
    linewidth = 1
  ) +
  labs(x = "h (transferability)", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.continuous, file = "./results/sim_htrends_outlier_continuous_1se.pdf", width = 10, height = 8, units = "in", dpi = 300)

p.continuous.rss <- output_continuous_h %>% 
  group_by(h, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = h, y = 1-mean_mean_R2_out, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = 1-mean_mean_R2_out - mean_se_R2_out,
      ymax = 1-mean_mean_R2_out + mean_se_R2_out
    ),
    width = 0.01,
    linewidth = 1
  ) +
  labs(x = "h (transferability)", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.continuous.rss, file = "./results/sim_htrends_outlier_continuous_rss_1se.pdf", width = 10, height = 8, units = "in", dpi = 300)

output_categorical_h <- bind_rows(output_categorical_mean_h0, 
                               output_categorical_mean_h0.25,
                               output_categorical_mean_h0.5,
                               output_categorical_mean_h0.75,
                               output_categorical_mean_h1,
                               output_categorical_mean_h1.25,
                               output_categorical_mean_h1.5,
                               .id = "h")
output_categorical_h$h <- rep(h, each = nrow(output_categorical_mean_h0))

p.categorical <- output_categorical_h %>% 
  group_by(h, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = h, y = mean_mean_Accuracy, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Accuracy - mean_se_Accuracy,
      ymax = mean_mean_Accuracy + mean_se_Accuracy
    ),
    width = 0.01,
    linewidth = 1
  ) +
  labs(x = "h (transferability)", y = "Accuracy") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.categorical, file = "./results/sim_htrends_outlier_categorical_1se.pdf", width = 10, height = 8, units = "in", dpi = 300)

output_coeff_h <- bind_rows(output_coeff_mean_h0, 
  output_coeff_mean_h0.25,
  output_coeff_mean_h0.5,
  output_coeff_mean_h0.75,
  output_coeff_mean_h1,
  output_coeff_mean_h1.25,
  output_coeff_mean_h1.5,
  .id = "h")
output_coeff_h$h <- rep(h, each = nrow(output_coeff_mean_h0))

p.coeff <- output_coeff_h %>% 
  group_by(h, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = h, y = mean_mean_Beta_MSE, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Beta_MSE - mean_se_Beta_MSE,
      ymax = mean_mean_Beta_MSE + mean_se_Beta_MSE
    ),
    width = 0.01,
    linewidth = 1
  ) +
  labs(x = "h (transferability)", y = "Coefficients Estimation MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.coeff, file = "./results/sim_htrends_outlier_coeff_1se.pdf", width = 10, height = 8, units = "in", dpi = 300)

output_coeff_con_h <- bind_rows(output_coeff_mean_con_h0, 
  output_coeff_mean_con_h0.25,
  output_coeff_mean_con_h0.5,
  output_coeff_mean_con_h0.75,
  output_coeff_mean_con_h1,
  output_coeff_mean_con_h1.25,
  output_coeff_mean_con_h1.5,
  .id = "h")
output_coeff_con_h$h <- rep(h, each = nrow(output_coeff_mean_con_h0))

p.coeff_con <- output_coeff_con_h %>% 
  group_by(h, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = h, y = mean_mean_Beta_MSE, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Beta_MSE - mean_se_Beta_MSE,
      ymax = mean_mean_Beta_MSE + mean_se_Beta_MSE
    ),
    width = 0.01,
    linewidth = 1
  ) +
  labs(x = "h (transferability)", y = "Coefficients Estimation MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.coeff_con, file = "./results/sim_htrends_outlier_coeff_con_1se.pdf", width = 10, height = 8, units = "in", dpi = 300)

output_coeff_cat_h <- bind_rows(output_coeff_mean_cat_h0, 
  output_coeff_mean_cat_h0.25,
  output_coeff_mean_cat_h0.5,
  output_coeff_mean_cat_h0.75,
  output_coeff_mean_cat_h1,
  output_coeff_mean_cat_h1.25,
  output_coeff_mean_cat_h1.5,
  .id = "h")
output_coeff_cat_h$h <- rep(h, each = nrow(output_coeff_mean_cat_h0))

p.coeff_cat <- output_coeff_cat_h %>% 
  group_by(h, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = h, y = mean_mean_Beta_MSE, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Beta_MSE - mean_se_Beta_MSE,
      ymax = mean_mean_Beta_MSE + mean_se_Beta_MSE
    ),
    width = 0.01,
    linewidth = 1
  ) +
  labs(x = "h (transferability)", y = "Coefficients Estimation MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.coeff_cat, file = "./results/sim_htrends_outlier_coeff_cat_1se.pdf", width = 10, height = 8, units = "in", dpi = 300)


p.combined <- patchwork::wrap_plots(
  p.continuous.rss,
  p.categorical,
  p.coeff_con,
  p.coeff_cat,
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
  file = "./results/sim_htrends_outlier_combined_2x2_1se.pdf",
  width = 16,
  height = 16,
  units = "in",
  dpi = 300
)
