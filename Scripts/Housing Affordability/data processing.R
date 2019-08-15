#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl")
pack(packages)
rm(pack, packages)

#import raw data
source("data import.R")

#create region, subregion and planning area tables
afford_region <- afford_county %>% 
  group_by(year, ownership, cost_bracket) %>%
  summarize(n = sum(n), class = first(class), expensive = first(expensive))

afford_subregion <- afford_county %>% 
  group_by(subregion, year, ownership, cost_bracket) %>%
  summarize(n = sum(n), class = first(class), expensive = first(expensive))

afford_PA <- afford_munic %>% 
  group_by(planning_area, year, ownership, cost_bracket) %>%
  summarize(n = sum(n), class = first(class), expensive = first(expensive))

#calculate percent of population spending about 35% on housing for each geography
expensive_fun <- function(df, name){
  name <- enquo(name)

  total <- df %>% group_by(!!name, ownership, year) %>% 
    summarize(total = sum(n))
  
  above_35 <- df %>% filter(expensive == "Above 35%") %>% 
    group_by(!!name, ownership, year) %>% 
    summarize(expensive = sum(n))
  
  bind_cols(total, above_35) %>%
    mutate(ratio = expensive / total) %>%
    select(!!name, ownership, year, expensive, total, ratio) %>%
    ungroup()}

ratio_county <- expensive_fun(afford_county, co_name) 
ratio_munic <- expensive_fun(afford_munic, munic_name)
ratio_subregion <- expensive_fun(afford_subregion, subregion)
ratio_PA <- expensive_fun(afford_PA, planning_area)

region_total <- afford_region %>% group_by(ownership, year) %>% 
  summarize(total = sum(n)) 
region_above_35 <- afford_region %>% filter(expensive == "Above 35%") %>% 
  group_by(ownership, year) %>% 
  summarize(expensive = sum(n))
ratio_region <- bind_cols(region_total, region_above_35) %>%
  mutate(ratio = expensive / total) %>%
  select(ownership, year, expensive, total, ratio) %>%
  ungroup()
rm(expensive_fun, region_total, region_above_35)

#overall ratio for each geography (Graph 1)
ovr_ratio_region <- ratio_region %>% group_by(year) %>% 
  summarize(expensive = sum(expensive), 
            total = sum(total), 
            ratio = expensive / total,
            geography = "DVRPC",
            geo_type = "region")

ovr_ratio_county <- ratio_county %>% group_by(year, co_name) %>% 
  summarize(expensive = sum(expensive), 
            total = sum(total), 
            ratio = expensive / total,
            geo_type = "county") %>%
  rename(geography = co_name)

ovr_ratio_subregion <- ratio_subregion %>% group_by(year, subregion) %>% 
  summarize(expensive = sum(expensive), 
            total = sum(total), 
            ratio = expensive / total,
            geo_type = "subregion") %>%
  rename(geography = subregion)

ovr_ratio_PA <- ratio_PA %>% group_by(year, planning_area) %>% 
  summarize(expensive = sum(expensive), 
            total = sum(total), 
            ratio = expensive / total,
            geo_type = "planning area") %>%
  rename(geography = planning_area)

ratio_all <- bind_rows(ovr_ratio_region, ovr_ratio_subregion, ovr_ratio_county, ovr_ratio_PA)
rm(ovr_ratio_region, ovr_ratio_county, ovr_ratio_subregion, ovr_ratio_PA)
