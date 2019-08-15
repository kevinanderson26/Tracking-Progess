#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl", "viridis")
pack(packages)
rm(pack, packages)

source("data import quarterly census.R")
source("data import cbp.R")

#calculuate annual change for qcew by county, create regional and subregional tables with 
qcew_county %<>% group_by(county) %>%
  mutate(annual_change = ifelse(year > "2001-01-01", (jobs - lag(jobs, 1)) / lag(jobs, 1), 0),
         total_change = (jobs - first(jobs)) / first(jobs)) %>%
  ungroup()

qcew_subregional <- qcew_county %>% group_by(year, subregion) %>%
  summarize(jobs = sum(jobs)) %>%
  ungroup() %>%
  group_by(subregion) %>%
  mutate(annual_change = ifelse(year > "2001-01-01", (jobs - lag(jobs, 1)) / lag(jobs, 1), 0),
         total_change = (jobs - first(jobs)) / first(jobs)) %>%
  ungroup()

qcew_regional <- qcew_county %>% group_by(year) %>%
  summarize(jobs = sum(jobs)) %>%
  ungroup() %>%
  mutate(annual_change = ifelse(year > "2001-01-01", (jobs - lag(jobs, 1)) / lag(jobs, 1), 0),
         total_change = (jobs - first(jobs)) / first(jobs))

ggplot(qcew_subregional, aes(x = year)) +
  geom_line(aes(y = annual_change, color = subregion)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(x = "Year", y = "Percent Change")

ggplot(qcew_subregional, aes(x = year)) +
  geom_line(aes(y = total_change, color = subregion)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(x = "Year", y = "Percent Change")

#calculate percentage of employment per county for each year
county_percent <- qcew_county %>% group_by(year) %>%
  mutate(percent = jobs / sum(jobs))

ggplot(county_percent) +
  geom_col(aes(x = year, y = percent, fill = county))

#add percentages to cbp table for each county, create regional and subregional cbp tables

cbp_county %<>% group_by(year, county_name) %>%
  mutate(emp_percent = employees / sum(employees)) %>%
  ungroup()

cbp_subregional <- cbp_county %>% group_by(year, subregion, sector) %>%
  summarize(employees = sum(employees)) %>%
  mutate(emp_percent = employees / sum(employees)) %>%
  ungroup()

cbp_regional <- cbp_county %>% group_by(year, sector) %>%
  summarize(employees = sum(employees)) %>%
  mutate(emp_percent = employees / sum(employees)) %>%
  ungroup()

cbp_regional %>%
  ggplot() +
  geom_col(aes(x = year, y = emp_percent, fill = sector)) +
  scale_fill_viridis_d() +
  scale_x_continuous(limits = c(2005, 2018))