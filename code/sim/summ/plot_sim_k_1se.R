rm(list = ls())
library(ggplot2)
library(tidyverse)


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

# k trend - OOB Variance####
iterations <- 100

k.trans <- c(2:10)
p <- 150

iterations <- 100

## k = 2 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k2_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k2","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k2 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k2 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k2 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k2 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k2 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k2 <- summarize_mtlmice_pool(output_mtlmice_pool_k2, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k2 <- summarize_mtlmice_pool(output_mtlmice_k2, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k2 <- summarize_mtlmice_pool(output_mtlmice0_k2, "mtlmice0", iterations = length(numbers))
output_mice_sum_k2 <- summarize_mtlmice_pool(output_mice_k2, "mice", iterations = length(numbers))
output_mice_pool_sum_k2 <- summarize_mtlmice_pool(output_mice_pool_k2, "mice_pool", iterations = length(numbers))


output_continuous_summary_k2 <- rbind(output_mtlmice_pool_sum_k2$Continuous,
                                       output_mtlmice_sum_k2$Continuous, 
                                       output_mtlmice0_sum_k2$Continuous, 
                                       output_mice_sum_k2$Continuous,
                                       output_mice_pool_sum_k2$Continuous)

output_coeff_summary_k2 <- rbind(output_mtlmice_pool_sum_k2$Coefficients,
                                       output_mtlmice_sum_k2$Coefficients, 
                                       output_mtlmice0_sum_k2$Coefficients, 
                                       output_mice_sum_k2$Coefficients,
                                       output_mice_pool_sum_k2$Coefficients)


output_continuous_mean_k2 <- output_continuous_summary_k2 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k2 <- output_coeff_summary_k2 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 3 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k3_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k3","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k3 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k3 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k3 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k3 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k3 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k3 <- summarize_mtlmice_pool(output_mtlmice_pool_k3, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k3 <- summarize_mtlmice_pool(output_mtlmice_k3, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k3 <- summarize_mtlmice_pool(output_mtlmice0_k3, "mtlmice0", iterations = length(numbers))
output_mice_sum_k3 <- summarize_mtlmice_pool(output_mice_k3, "mice", iterations = length(numbers))
output_mice_pool_sum_k3 <- summarize_mtlmice_pool(output_mice_pool_k3, "mice_pool", iterations = length(numbers))


output_continuous_summary_k3 <- rbind(output_mtlmice_pool_sum_k3$Continuous,
                                       output_mtlmice_sum_k3$Continuous, 
                                       output_mtlmice0_sum_k3$Continuous, 
                                       output_mice_sum_k3$Continuous,
                                       output_mice_pool_sum_k3$Continuous)

output_coeff_summary_k3 <- rbind(output_mtlmice_pool_sum_k3$Coefficients,
                                       output_mtlmice_sum_k3$Coefficients, 
                                       output_mtlmice0_sum_k3$Coefficients, 
                                       output_mice_sum_k3$Coefficients,
                                       output_mice_pool_sum_k3$Coefficients)


output_continuous_mean_k3 <- output_continuous_summary_k3 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k3 <- output_coeff_summary_k3 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 4 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k4_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k4","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k4 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k4 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k4 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k4 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k4 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k4 <- summarize_mtlmice_pool(output_mtlmice_pool_k4, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k4 <- summarize_mtlmice_pool(output_mtlmice_k4, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k4 <- summarize_mtlmice_pool(output_mtlmice0_k4, "mtlmice0", iterations = length(numbers))
output_mice_sum_k4 <- summarize_mtlmice_pool(output_mice_k4, "mice", iterations = length(numbers))
output_mice_pool_sum_k4 <- summarize_mtlmice_pool(output_mice_pool_k4, "mice_pool", iterations = length(numbers))


output_continuous_summary_k4 <- rbind(output_mtlmice_pool_sum_k4$Continuous,
                                       output_mtlmice_sum_k4$Continuous, 
                                       output_mtlmice0_sum_k4$Continuous, 
                                       output_mice_sum_k4$Continuous,
                                       output_mice_pool_sum_k4$Continuous)

output_coeff_summary_k4 <- rbind(output_mtlmice_pool_sum_k4$Coefficients,
                                       output_mtlmice_sum_k4$Coefficients, 
                                       output_mtlmice0_sum_k4$Coefficients, 
                                       output_mice_sum_k4$Coefficients,
                                       output_mice_pool_sum_k4$Coefficients)


output_continuous_mean_k4 <- output_continuous_summary_k4 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k4 <- output_coeff_summary_k4 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 5 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k5_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k5","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k5 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k5 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k5 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k5 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k5 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k5 <- summarize_mtlmice_pool(output_mtlmice_pool_k5, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k5 <- summarize_mtlmice_pool(output_mtlmice_k5, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k5 <- summarize_mtlmice_pool(output_mtlmice0_k5, "mtlmice0", iterations = length(numbers))
output_mice_sum_k5 <- summarize_mtlmice_pool(output_mice_k5, "mice", iterations = length(numbers))
output_mice_pool_sum_k5 <- summarize_mtlmice_pool(output_mice_pool_k5, "mice_pool", iterations = length(numbers))


output_continuous_summary_k5 <- rbind(output_mtlmice_pool_sum_k5$Continuous,
                                       output_mtlmice_sum_k5$Continuous, 
                                       output_mtlmice0_sum_k5$Continuous, 
                                       output_mice_sum_k5$Continuous,
                                       output_mice_pool_sum_k5$Continuous)

output_coeff_summary_k5 <- rbind(output_mtlmice_pool_sum_k5$Coefficients,
                                       output_mtlmice_sum_k5$Coefficients, 
                                       output_mtlmice0_sum_k5$Coefficients, 
                                       output_mice_sum_k5$Coefficients,
                                       output_mice_pool_sum_k5$Coefficients)


output_continuous_mean_k5 <- output_continuous_summary_k5 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k5 <- output_coeff_summary_k5 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )


## k = 6 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k6_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k6","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k6 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k6 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k6 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k6 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k6 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k6 <- summarize_mtlmice_pool(output_mtlmice_pool_k6, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k6 <- summarize_mtlmice_pool(output_mtlmice_k6, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k6 <- summarize_mtlmice_pool(output_mtlmice0_k6, "mtlmice0", iterations = length(numbers))
output_mice_sum_k6 <- summarize_mtlmice_pool(output_mice_k6, "mice", iterations = length(numbers))
output_mice_pool_sum_k6 <- summarize_mtlmice_pool(output_mice_pool_k6, "mice_pool", iterations = length(numbers))


output_continuous_summary_k6 <- rbind(output_mtlmice_pool_sum_k6$Continuous,
                                       output_mtlmice_sum_k6$Continuous, 
                                       output_mtlmice0_sum_k6$Continuous, 
                                       output_mice_sum_k6$Continuous,
                                       output_mice_pool_sum_k6$Continuous)

output_coeff_summary_k6 <- rbind(output_mtlmice_pool_sum_k6$Coefficients,
                                       output_mtlmice_sum_k6$Coefficients, 
                                       output_mtlmice0_sum_k6$Coefficients, 
                                       output_mice_sum_k6$Coefficients,
                                       output_mice_pool_sum_k6$Coefficients)


output_continuous_mean_k6 <- output_continuous_summary_k6 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k6 <- output_coeff_summary_k6 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 7 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k7_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k7","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k7 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k7 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k7 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k7 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k7 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k7 <- summarize_mtlmice_pool(output_mtlmice_pool_k7, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k7 <- summarize_mtlmice_pool(output_mtlmice_k7, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k7 <- summarize_mtlmice_pool(output_mtlmice0_k7, "mtlmice0", iterations = length(numbers))
output_mice_sum_k7 <- summarize_mtlmice_pool(output_mice_k7, "mice", iterations = length(numbers))
output_mice_pool_sum_k7 <- summarize_mtlmice_pool(output_mice_pool_k7, "mice_pool", iterations = length(numbers))


output_continuous_summary_k7 <- rbind(output_mtlmice_pool_sum_k7$Continuous,
                                       output_mtlmice_sum_k7$Continuous, 
                                       output_mtlmice0_sum_k7$Continuous, 
                                       output_mice_sum_k7$Continuous,
                                       output_mice_pool_sum_k7$Continuous)

output_coeff_summary_k7 <- rbind(output_mtlmice_pool_sum_k7$Coefficients,
                                       output_mtlmice_sum_k7$Coefficients, 
                                       output_mtlmice0_sum_k7$Coefficients, 
                                       output_mice_sum_k7$Coefficients,
                                       output_mice_pool_sum_k7$Coefficients)


output_continuous_mean_k7 <- output_continuous_summary_k7 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k7 <- output_coeff_summary_k7 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 8 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k8_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k8","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k8 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k8 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k8 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k8 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k8 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k8 <- summarize_mtlmice_pool(output_mtlmice_pool_k8, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k8 <- summarize_mtlmice_pool(output_mtlmice_k8, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k8 <- summarize_mtlmice_pool(output_mtlmice0_k8, "mtlmice0", iterations = length(numbers))
output_mice_sum_k8 <- summarize_mtlmice_pool(output_mice_k8, "mice", iterations = length(numbers))
output_mice_pool_sum_k8 <- summarize_mtlmice_pool(output_mice_pool_k8, "mice_pool", iterations = length(numbers))


output_continuous_summary_k8 <- rbind(output_mtlmice_pool_sum_k8$Continuous,
                                       output_mtlmice_sum_k8$Continuous, 
                                       output_mtlmice0_sum_k8$Continuous, 
                                       output_mice_sum_k8$Continuous,
                                       output_mice_pool_sum_k8$Continuous)

output_coeff_summary_k8 <- rbind(output_mtlmice_pool_sum_k8$Coefficients,
                                       output_mtlmice_sum_k8$Coefficients, 
                                       output_mtlmice0_sum_k8$Coefficients, 
                                       output_mice_sum_k8$Coefficients,
                                       output_mice_pool_sum_k8$Coefficients)


output_continuous_mean_k8 <- output_continuous_summary_k8 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k8 <- output_coeff_summary_k8 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 9 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k9_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k9","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k9 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k9 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k9 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k9 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k9 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k9 <- summarize_mtlmice_pool(output_mtlmice_pool_k9, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k9 <- summarize_mtlmice_pool(output_mtlmice_k9, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k9 <- summarize_mtlmice_pool(output_mtlmice0_k9, "mtlmice0", iterations = length(numbers))
output_mice_sum_k9 <- summarize_mtlmice_pool(output_mice_k9, "mice", iterations = length(numbers))
output_mice_pool_sum_k9 <- summarize_mtlmice_pool(output_mice_pool_k9, "mice_pool", iterations = length(numbers))


output_continuous_summary_k9 <- rbind(output_mtlmice_pool_sum_k9$Continuous,
                                       output_mtlmice_sum_k9$Continuous, 
                                       output_mtlmice0_sum_k9$Continuous, 
                                       output_mice_sum_k9$Continuous,
                                       output_mice_pool_sum_k9$Continuous)

output_coeff_summary_k9 <- rbind(output_mtlmice_pool_sum_k9$Coefficients,
                                       output_mtlmice_sum_k9$Coefficients, 
                                       output_mtlmice0_sum_k9$Coefficients, 
                                       output_mice_sum_k9$Coefficients,
                                       output_mice_pool_sum_k9$Coefficients)


output_continuous_mean_k9 <- output_continuous_summary_k9 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k9 <- output_coeff_summary_k9 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## k = 10 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k10_([0-9]+)\\.RData$", "\\1",file_names)))

mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)


for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k10","_", i, ".RData")
  load(file_name)
  
  mtlmice_pool_list[[i]] <- output_mtlmice_pool
  
  mtlmice_list[[i]] <- output_mtlmice
  
  mtlmice0_list[[i]] <- output_mtlmice0
  
  mice_list[[i]] <- output_mice
  
  mice_pool_list[[i]] <- output_mice_pool
  
}

output_mtlmice_pool_k10 <- Filter(function(x) !is.character(x), mtlmice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice_k10 <- Filter(function(x) !is.character(x), mtlmice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mtlmice0_k10 <- Filter(function(x) !is.character(x), mtlmice0_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_k10 <- Filter(function(x) !is.character(x), mice_list) %>% 
  Filter(function(x) !is.null(x), .) 
output_mice_pool_k10 <- Filter(function(x) !is.character(x), mice_pool_list) %>% 
  Filter(function(x) !is.null(x), .) 

output_mtlmice_pool_sum_k10 <- summarize_mtlmice_pool(output_mtlmice_pool_k10, method = "mtlmice_pool", iterations = length(numbers))
output_mtlmice_sum_k10 <- summarize_mtlmice_pool(output_mtlmice_k10, "mtlmice", iterations = length(numbers))
output_mtlmice0_sum_k10 <- summarize_mtlmice_pool(output_mtlmice0_k10, "mtlmice0", iterations = length(numbers))
output_mice_sum_k10 <- summarize_mtlmice_pool(output_mice_k10, "mice", iterations = length(numbers))
output_mice_pool_sum_k10 <- summarize_mtlmice_pool(output_mice_pool_k10, "mice_pool", iterations = length(numbers))


output_continuous_summary_k10 <- rbind(output_mtlmice_pool_sum_k10$Continuous,
                                       output_mtlmice_sum_k10$Continuous, 
                                       output_mtlmice0_sum_k10$Continuous, 
                                       output_mice_sum_k10$Continuous,
                                       output_mice_pool_sum_k10$Continuous)

output_coeff_summary_k10 <- rbind(output_mtlmice_pool_sum_k10$Coefficients,
                                       output_mtlmice_sum_k10$Coefficients, 
                                       output_mtlmice0_sum_k10$Coefficients, 
                                       output_mice_sum_k10$Coefficients,
                                       output_mice_pool_sum_k10$Coefficients)


output_continuous_mean_k10 <- output_continuous_summary_k10 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

output_coeff_mean_k10 <- output_coeff_summary_k10 %>% 
  group_by(Task, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("se"), mean, na.rm = TRUE, .names = "mean_{.col}")
  )

## Plot ####
output_continuous_k <- bind_rows(output_continuous_mean_k2, 
                               output_continuous_mean_k3,
                               output_continuous_mean_k4,
                               output_continuous_mean_k5,
                               output_continuous_mean_k6,
                               output_continuous_mean_k7,
                               output_continuous_mean_k8,
                               output_continuous_mean_k9,
                               output_continuous_mean_k10,
                               .id = "k")
output_continuous_k$k <- as.numeric(output_continuous_k$k) + 1

p.continuous <- output_continuous_k %>% 
  group_by(k, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = k, y = mean_mean_MSE_rel, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_MSE_rel - mean_se_MSE_rel,
      ymax = mean_mean_MSE_rel + mean_se_MSE_rel
    ),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "k (Numbers of tasks)", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.continuous, file = "./results/sim_ktrends_1se_continuous.pdf", width = 10, height = 8, units = "in", dpi = 300)


p.continuous.rss <- output_continuous_k %>% 
  group_by(k, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = k, y = 1-mean_mean_R2_out, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = 1-mean_mean_R2_out - mean_se_R2_out,
      ymax = 1-mean_mean_R2_out + mean_se_R2_out
    ),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "k (Numbers of tasks)", y = "Imputation Relative MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.continuous.rss, file = "./results/sim_ktrends_1se_continuous_rss.pdf", width = 10, height = 8, units = "in", dpi = 300)

output_coeff_k <- bind_rows(output_coeff_mean_k2, 
  output_coeff_mean_k3,
  output_coeff_mean_k4,
  output_coeff_mean_k5,
  output_coeff_mean_k6,
  output_coeff_mean_k7,
  output_coeff_mean_k8,
  output_coeff_mean_k9,
  output_coeff_mean_k10,
  .id = "k")
output_coeff_k$k <- as.numeric(output_coeff_k$k) + 1

p.coeff <- output_coeff_k %>% 
  group_by(k, method, type_group) %>%
  summarise(
    across(starts_with("mean"), mean, na.rm = TRUE),
    across(starts_with("se"), mean, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = k, y = mean_mean_Beta_MSE, color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = mean_mean_Beta_MSE - mean_se_Beta_MSE,
      ymax = mean_mean_Beta_MSE + mean_se_Beta_MSE
    ),
    width = 0.01,
    linewidth = 0.4
  ) +
  labs(x = "k (Numbers of tasks)", y = "Coefficients Estimation MSE") +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(p.coeff, file = "./results/sim_ktrends_1se_coeff.pdf", width = 10, height = 8, units = "in", dpi = 300)

p.combined <- patchwork::wrap_plots(
  p.continuous.rss,
  p.coeff,
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
  file = "./results/sim_ktrends_1se_continuous_rss_coeff.pdf",
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

rm(list = ls())
library(ggplot2)
library(tidyverse)


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
  mtlmice_pool = "MTL-MICE-Pool",
  mtlmice      = "MTL-MICE",
  mtlmice0     = "MTL-MICE-0",
  mice         = "MICE",
  mice_pool   = "MICE-Pool"
)

# k trend - Check Variance and covariance####
iterations <- 100

k.trans <- c(2:10)
p <- 150

iterations <- 100

## k = 2 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k2_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice_pool_list <- mtlmice_list <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)
dat_cov_list <- mtlmice_pool_cov_list <- mtlmice_cov_list <- mtlmice0_cov_list <- mice_cov_list <- mice_pool_cov_list  <- vector("list", iterations)

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k2","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))

    dat_cov_list[[i]] <- mean(unlist(lapply(dat, function(x){
    cov(x$data[,"ic_x1"], x$data[,"y"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k2 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

output_cov_k2 <- data.frame(
  Cov = c(mean(unlist(dat_cov_list)), mean(unlist(mtlmice_pool_cov_list)), mean(unlist(mtlmice_cov_list)), mean(unlist(mtlmice0_cov_list)), mean(unlist(mice_cov_list)), mean(unlist(mice_pool_cov_list))),
  sd = c(sd(unlist(dat_cov_list)), sd(unlist(mtlmice_pool_cov_list)), sd(unlist(mtlmice_cov_list)), sd(unlist(mtlmice0_cov_list)), sd(unlist(mice_cov_list)), sd(unlist(mice_pool_cov_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 3 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k3_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k3","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))

  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k3 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 4 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k4_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k4","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k4 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 5 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k5_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k5","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k5 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 6 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k6_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k6","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k6 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 7 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k7_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k7","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k7 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 8 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k8_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k8","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k8 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 9 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k9_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k9","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k9 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

## k = 10 ####

file_names <- dir(path = "/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends", pattern = "*.RData")
numbers <- na.omit(as.numeric(gsub("^k_1se_k10_([0-9]+)\\.RData$", "\\1",file_names)))

dat_list <- mtlmice <- mtlmice0_list <- mice_list <- mice_pool_list  <- vector("list", iterations)  

for(i in numbers){
  
  file_name <- paste0("/Users/yuyuchen/Library/CloudStorage/Dropbox/New York University/Research/Feng Lab/MTL-MICE/sim/output/ktrends/k_1se_k10","_", i, ".RData")
  load(file_name)

  dat_list[[i]] <- mean(unlist(lapply(dat, function(x){
    var(x$data[,"ic_x1"])
  })))
  
  mtlmice_pool_list[[i]] <- mean(unlist(lapply(mtlmice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice_list[[i]] <- mean(unlist(lapply(mtlmice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mtlmice0_list[[i]] <- mean(unlist(lapply(mtlmice0.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_list[[i]] <- mean(unlist(lapply(mice.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
  mice_pool_list[[i]] <- mean(unlist(lapply(mice.pool.imp$dat.imp.final, function(x){
    lapply(x, function(y) var(y[, "ic_x1"]))
  })))
  
}

output_var_k10 <- data.frame(
  Var = c(mean(unlist(dat_list)), mean(unlist(mtlmice_pool_list)), mean(unlist(mtlmice_list)), mean(unlist(mtlmice0_list)), mean(unlist(mice_list)), mean(unlist(mice_pool_list))),
  sd = c(sd(unlist(dat_list)), sd(unlist(mtlmice_pool_list)), sd(unlist(mtlmice_list)), sd(unlist(mtlmice0_list)), sd(unlist(mice_list)), sd(unlist(mice_pool_list))),
  method = c("dat", "mtlmice_pool", "mtlmice", "mtlmice0", "mice", "mice_pool")
)

output_var <- bind_rows(output_var_k2, output_var_k3, output_var_k4, output_var_k5, output_var_k6, output_var_k7, output_var_k8, output_var_k9, output_var_k10, .id = "k")

output_var$method <- factor(output_var$method)
output_var$k <- as.numeric(output_var$k)+1

output_var %>% 
  group_by(method) %>%
  summarise(
    across(starts_with("Var"), mean, na.rm = TRUE, .names = "mean_{.col}"),
    across(starts_with("sd"), mean, na.rm = TRUE, .names = "mean_Var_{.col}")
  )