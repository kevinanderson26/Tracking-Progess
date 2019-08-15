#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "sf", "tidycensus", "readxl", "units", "scales")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import data
source("1) Data Import.R")

#add segment length, interstate and road_type fields to New Jersey dataframe
#Note: NJ data is originally in lane miles, but being converted to segment miles here to be comparable with PA data

nj_pavement %<>%
  mutate(length = 0.1, 
         nhs = ifelse(nhs == "Y", TRUE, FALSE),
         interstate = ifelse(str_detect(route_name, "^I"), TRUE, FALSE),
         road_type = case_when(nhs == TRUE & interstate == TRUE ~ "Interstate",
                               nhs == TRUE & interstate == FALSE ~ "NHS, non-interstate",
                               nhs == FALSE & avg_annual_daily_traffic >= 2000 ~ "Non-NHS, >= 2000 ADT",
                               nhs == FALSE & avg_annual_daily_traffic < 2000 ~ "Non-NHS, < 2000 ADT",
                               nhs == FALSE & is.na(avg_annual_daily_traffic) ~ "Non-NHS, < 2000 ADT"))

#calculate pavement rating for each NJ road segment
nj_pavement %<>%
  mutate(pavement_quality = ifelse(int_roughness_index < 95 & surface_distress_index >= 3.5, "Good",
                                   ifelse(int_roughness_index > 170 | surface_distress_index <= 2.4, "Poor",
                                          "Fair")))

#summarize NJ pavement data
nj_pavement_summary <- nj_pavement %>%
  group_by(year, road_type, pavement_quality) %>%
  summarize(miles = sum(length),
            state = "New Jersey") %>%
  filter(!is.na(pavement_quality)) %>%
  ungroup()

# summarize PA pavement data and recalibrate PA data to collapse good and excellent categories 
# to make directly comparable with NJ categories
pa_pavement_summary %<>%
  mutate(pavement_quality_adj = ifelse(pavement_quality == "Excellent", "Good", 
                                       as.character(pavement_quality))) %>%
  group_by(year, road_type, pavement_quality_adj) %>%
  summarize(miles = sum(miles)) %>%
  rename(pavement_quality = pavement_quality_adj) %>%
  mutate(state = "Pennsylvania") %>%
  ungroup()

#recode PA and NJ data to combine non-NHS road types

nj_pavement_summary %<>% 
  mutate(road_type_adjusted = case_when(road_type == "Non-NHS, < 2000 ADT" ~ "Non-NHS",
                                        road_type == "Non-NHS, >= 2000 ADT" ~ "Non-NHS",
                                        road_type == "Interstate" ~ "NHS, interstate",
                                        TRUE ~ road_type)) %>%
  select(year, state, road_type_adjusted, pavement_quality, miles) %>%
  group_by(year, state, road_type_adjusted, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  ungroup()

pa_pavement_summary %<>% 
  mutate(road_type_adjusted = case_when(road_type == "Non-NHS, < 2000 ADT" ~ "Non-NHS",
                                        road_type == "Non-NHS, >= 2000 ADT" ~ "Non-NHS",
                                        road_type == "Interstate" ~ "NHS, interstate",
                                        TRUE ~ road_type)) %>%
  select(year, state, road_type_adjusted, pavement_quality, miles) %>%
  group_by(year, state, road_type_adjusted, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  ungroup()

#join PA and NJ summary data, convert pavement quality and road type to factors
summary_pavement <- bind_rows(pa_pavement_summary, nj_pavement_summary) %>%
  select(year, state, everything()) %>%
  mutate(pavement_quality = as.factor(pavement_quality),
         road_type_adjusted = as.factor(road_type_adjusted))

#add combined state rows 
summary_pavement %<>%
  group_by(year, road_type_adjusted, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  mutate(state = "DVRPC") %>%
  bind_rows(summary_pavement)

#add percent deficient for each state and road type and reorder variables
summary_pavement %<>% 
  group_by(year, state, road_type_adjusted) %>%
  mutate(percent = miles / sum(miles)) %>%
  select(year, state, road_type_adjusted, pavement_quality, miles, percent)