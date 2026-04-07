rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(tidyverse)
library(UpSetR)
library(readr)
library(foreach)
library(glmnet)
library(caret)
library(doParallel)
library(parallel)
library(tidyverse)
library(caret)

# Run this script from the repository root so these relative paths resolve.
scripts.sources <-  list.files("./code/R",
                               pattern="*.R$", full.names=TRUE, 
                               ignore.case=TRUE)
sapply(scripts.sources,source,.GlobalEnv)

setwd("./data/uselection")

dir.create("bystate_2020", recursive = TRUE, showWarnings = FALSE)
for (dir_name in c("mar_0.1", "mar_0.2", "mar_0.3", "mar_0.4", "mar_0.5")) {
  dir.create(file.path("bystate_2020", dir_name), recursive = TRUE, showWarnings = FALSE)
}

# 2020####
uselect2020 <- read_csv("2020_US_County_Level_Presidential_Results.csv")
countyfacts2016 <- read_csv("county_facts_2016.csv")

uselect2020 <- uselect2020 %>%
  rename(fips = county_fips) %>% 
  mutate(
    fips = as.integer(fips),
    y = as.integer(per_dem > per_gop)
  ) %>% 
  dplyr::select(fips, y)
uselect_full2020 <- uselect2020 %>%
  left_join(countyfacts2016, by = c("fips")) %>%
  filter(state_abbreviation != "AK" & state_abbreviation != "DC") # Exclude AK and DC for consistency with previous data 

state_summary <- uselect_full2020 %>% 
  group_by(state_abbreviation) %>% 
  summarise(
    n = n(),
    dem_prop = mean(y == 1, na.rm = TRUE)
  ) %>% 
  filter(n >= 75, dem_prop >= 0.10, dem_prop <= 0.90)


uselect_full2020 <- uselect_full2020 %>% 
  dplyr::select(-fips, -area_name)

# uselect_full2020 <- uselect_full2020 %>%
#   mutate(womenfirm25 = ifelse(SBO015207 >=25, 0,1)) %>% 
#   dplyr::select(-SBO015207)

uselect_full2020 <- uselect_full2020 %>%
  mutate(housingunit10 = ifelse(HSG096213 >=10, 0,1)) %>% 
  dplyr::select(-HSG096213)

uselect_target <- uselect_full2020 %>% 
  filter(state_abbreviation %in% state_summary$state_abbreviation)

uselect_target_list <- split(uselect_target, uselect_target$state_abbreviation) %>% 
  lapply(function(df) df %>% dplyr::select(-state_abbreviation))

uselect_source <- uselect_full2020 %>% 
  filter(!(state_abbreviation %in% state_summary$state_abbreviation))

uselect_source_list <- split(uselect_source, uselect_source$state_abbreviation) %>% 
  lapply(function(df) df %>% dplyr::select(-state_abbreviation))

uselect_list <- c(uselect_target_list, uselect_source_list)

save(uselect_target_list, uselect_source_list, uselect_list, file = "bystate_2020/uselect_list2020.RData")

for(i in 1:500){
  set.seed(i)
  # uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "EDU635213", prop.missing = 0.1, where.missing = c("womenfirm25", "HSG495213"))
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "EDU635213", prop.missing = 0.1, where.missing = c("housingunit10", "HSG495213"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate_2020/mar_0.1/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}


for(i in 1:1000){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "EDU635213", prop.missing = 0.2, where.missing = c("housingunit10", "HSG495213"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate_2020/mar_0.2/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:1000){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "EDU635213", prop.missing = 0.3, where.missing = c("housingunit10", "HSG495213"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate_2020/mar_0.3/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "EDU635213", prop.missing = 0.4, where.missing = c("housingunit10", "HSG495213"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate_2020/mar_0.4/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "EDU635213", prop.missing = 0.5, where.missing = c("housingunit10", "HSG495213"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate_2020/mar_0.5/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

# 2024####
required_2024_files <- c(
  "2024_US_County_Level_Presidential_Results.csv",
  "Education2023.csv",
  "PopulationEstimates.csv",
  "Unemployment2023.csv",
  "Poverty2023.csv"
)

if (all(file.exists(required_2024_files))) {
uselect2024 <- read_csv("2024_US_County_Level_Presidential_Results.csv")
edu2023 <- read_csv("Education2023.csv")
pop2023 <- read_csv("PopulationEstimates.csv")
unemploy2023 <- read_csv("Unemployment2023.csv")
pov2023 <- read_csv("Poverty2023.csv")

# en dash note: both "–" and "-" are handled
year_pat <- ",\\s*([0-9]{4}(?:[–-][0-9]{2,4})?)$"
edu2023 <- edu2023 %>%
  mutate(
    Year = str_match(Attribute, year_pat)[, 2],                 # capture the year/range
    Attribute = str_replace(Attribute, year_pat, "")            # drop the trailing ", year"
  ) %>% 
  filter(Year == "2019-23") %>%
  pivot_wider(
    id_cols = c(`FIPS Code`, State, `Area name`, Year),
    names_from = Attribute,
    values_from = Value
  ) %>% 
  rename(FIPS_Code = `FIPS Code`, Area_Name = `Area name`) %>% 
  dplyr::select(-Year, -State, -Area_Name)

pop2023 <- pop2023 %>%
  mutate(
    # Extract the first 4-digit year that appears anywhere in the string
    Year = str_extract(Attribute, "\\d{4}"),
    
    # Remove the year (and any surrounding underscores) from the attribute name
    Attribute = str_replace_all(Attribute, "_?\\d{4}_?", "_")
  ) %>%
  mutate(
    # Clean up multiple underscores or trailing underscores
    Attribute = str_replace_all(Attribute, "_{2,}", "_"),
    Attribute = str_replace(Attribute, "_$", "")
  ) %>% 
  filter(Year == "2023") %>%
  pivot_wider(
    id_cols = c(FIPStxt, State, Area_Name, Year),
    names_from = Attribute,
    values_from = Value
  ) %>% 
  rename(FIPS_Code = FIPStxt) %>% 
  dplyr::select(-Year, -State, -Area_Name, -Rural_Urban_Continuum_Code)

unemploy2023 <- unemploy2023 %>%
  mutate(
    # Extract the first 4-digit year that appears anywhere in the string
    Year = str_extract(Attribute, "\\d{4}"),
    
    # Remove the year (and any surrounding underscores) from the attribute name
    Attribute = str_replace_all(Attribute, "_?\\d{4}_?", "_")
  ) %>%
  mutate(
    # Clean up multiple underscores or trailing underscores
    Attribute = str_replace_all(Attribute, "_{2,}", "_"),
    Attribute = str_replace(Attribute, "_$", ""),
    Area_Name = str_replace(Area_Name, ",\\s*[A-Z]{2}$", "")
  ) %>% 
  filter(Year == "2023" | Attribute == "Median_Household_Income") %>%
  dplyr::select(-Year, -State, -Area_Name) %>%
  pivot_wider(
    id_cols = c(FIPS_Code),
    names_from = Attribute,
    values_from = Value
  ) %>% 
  dplyr::select(FIPS_Code, Unemployment_rate, Median_Household_Income)

pov2023 <- pov2023 %>%
  mutate(
    # Extract the first 4-digit year that appears anywhere in the string
    Year = str_extract(Attribute, "\\d{4}"),
    
    # Remove the year (and any surrounding underscores) from the attribute name
    Attribute = str_replace_all(Attribute, "_?\\d{4}_?", "_")
  ) %>%
  mutate(
    # Clean up multiple underscores or trailing underscores
    Attribute = str_replace_all(Attribute, "_{2,}", "_"),
    Attribute = str_replace(Attribute, "_$", "")
  ) %>% 
  rename(State = Stabr) %>%
  filter(Year == "2023") %>%
  pivot_wider(
    id_cols = c(FIPS_Code, State, Area_Name, Year),
    names_from = Attribute,
    values_from = Value
  ) %>% 
  dplyr::select(-Year, -State, -Area_Name) %>% 
  dplyr::select(FIPS_Code, PCTPOVALL, PCTPOV017)

uselect2024 <- uselect2024 %>%
  rename(State = state_name, FIPS_Code = county_fips, Area_Name = county_name) %>% 
  mutate(FIPS_Code = as.numeric(FIPS_Code))

# Merge datasets
uselect_full <- uselect2024 %>%
  left_join(edu2023, by = c("FIPS_Code")) %>%
  left_join(pop2023, by = c("FIPS_Code")) %>%
  left_join(unemploy2023, by = c("FIPS_Code")) %>%
  left_join(pov2023, by = c("FIPS_Code")) %>% 
  filter(State != "Alaska" & State != "District of Columbia")  %>% # Exclude AK and DC for consistency with previous data %>% 
  mutate(
    y = as.integer(per_dem > per_gop)
  )


state_summary <- uselect_full %>% 
  group_by(State) %>% 
  summarise(
    n = n(),
    dem_prop = mean(y == 1, na.rm = TRUE)
  ) %>% 
  filter(n >= 75, dem_prop >= 0.10, dem_prop <= 0.90)


uselect_full <- uselect_full %>% 
  dplyr::select(-FIPS_Code, -Area_Name, -votes_gop, -votes_dem, -diff, -per_dem, -per_gop, -total_votes, -per_point_diff,
         -`Less than high school graduate`, -`Some college or associate degree`, -`Bachelor's degree or higher`)

uselect_target <- uselect_full %>% 
  filter(State %in% state_summary$State)

uselect_target_list <- split(uselect_target, uselect_target$State) %>% 
  lapply(function(df) df %>% dplyr::select(-State))

uselect_source <- uselect_full %>% 
  filter(!(State %in% state_summary$State))

uselect_source_list <- split(uselect_source, uselect_source$State) %>% 
  lapply(function(df) df %>% dplyr::select(-State))

uselect_list <- c(uselect_target_list, uselect_source_list)

dir.create("bystate", recursive = TRUE, showWarnings = FALSE)
for (dir_name in c("mar_0.1", "mar_0.15", "mar_0.2", "mar_0.25", "mar_0.3", "mar_0.4")) {
  dir.create(file.path("bystate", dir_name), recursive = TRUE, showWarnings = FALSE)
}

save(uselect_target_list, uselect_source_list, uselect_list, file = "bystate/uselect_list.RData")

uselect_full_list <- split(uselect_full, uselect_full$State) %>% 
  lapply(function(df) df %>% dplyr::select(-State))

uselect_full_mar <- mar.sim(data = uselect_full_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.1, where.missing = c("y"))

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.1, where.missing = c("y"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate/mar_0.1/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.15, where.missing = c("y"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate/mar_0.15/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}


for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.2, where.missing = c("y"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate/mar_0.2/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.25, where.missing = c("y"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate/mar_0.25/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.3, where.missing = c("y"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate/mar_0.3/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}

for(i in 1:500){
  set.seed(i)
  uselect_target_mar <- mar.sim(data = uselect_target_list, covaraite = "Percent of adults who are not high school graduates", prop.missing = 0.4, where.missing = c("y"))
  uselect_mar <- c(uselect_target_mar, uselect_source_list)
  file_name <- paste0("bystate/mar_0.4/uselect_mar_", i, ".RData")
  save(uselect_mar, file = file_name)
}
} else {
  missing_2024_files <- required_2024_files[!file.exists(required_2024_files)]
  message(
    "Skipping 2024 election preprocessing block because these files are not present in data/uselection/: ",
    paste(missing_2024_files, collapse = ", ")
  )
}
