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
source("Scripts/Bridge Conditions/1) Data Import.R")

#add column including lowest rating (of deck, superstructure, substructure, and culvert)
bridges_region %<>% 
  mutate(rating_lowest = pmin(rating_deck, rating_super, rating_sub, rating_culvert,
                              na.rm = TRUE))

#bin ratings into good/fair/poor based on lowest rating
bridges_region %<>%
  mutate(condition = ifelse(rating_lowest >= 7, "Good",
                            ifelse(rating_lowest >= 5, "Fair", 
                                   "Poor")))

#factor condition classes
bridges_region$condition <- factor(bridges_region$condition, 
                                   levels = c("Poor", "Fair", "Good"))

###manually adjust duplicated bi-state bridges by reducing area by half

#create new 'adjusted sq feet' variable to maintain original area
bridges_region$area_sqft_adj <- bridges_region$area_sqft

#lists of bridges to be modified
drpa_bridges <- c("237301032201190", "4500001", "15452", #commodore barry
                  "677301007602300", "4500003", "39190", #walt whitman
                  "677301009001200", "4500011", "39199", #betsy ross
                  "677301067600000", "4500010", "39265") #ben franklin

turnpike_bridges <- c("97276990359000", "P000000")

bcbc_bridges <- c("677301999100150", "3000001", "39272", #tacony-palmyra
                  "97101999100010", "3000002", "7590") #burlington-bristol
    
#trim structure numbers for easier comparison
bridges_region %<>% mutate(structure_number = str_trim(structure_number))              

#halve area all of all bridges on lists
for(i in 1:length(bridges_region$structure_number)){
  #DRPA Bridges
  if(bridges_region$structure_number[i] %in% drpa_bridges & bridges_region$year[i] %in% c(2000:2013, 2018)){
    bridges_region$area_sqft_adj[i] <- bridges_region$area_sqft[i] / 2}
  #Turnpike Bridge
  if(bridges_region$structure_number[i] %in% turnpike_bridges & bridges_region$year[i] %in% c(2000:2007)){
    bridges_region$area_sqft_adj[i] <- bridges_region$area_sqft[i] / 2}
  #BCBC Bridges
  if(bridges_region$structure_number[i] %in% bcbc_bridges & bridges_region$year[i] %in% c(2000:2011)){
    bridges_region$area_sqft_adj[i] <- bridges_region$area_sqft[i] / 2}
}

#remove unneeded lists
rm(i, drpa_bridges, turnpike_bridges, bcbc_bridges)

# count bridges in deficient condition, by maintainer class, convert area to millions of square feeet,
# and determine percent of bridges and percent of area in each category 
condition_count_main <- bridges_region %>%
  filter(!is.na(condition)) %>%
  group_by(year, state, maintainer_class, condition) %>%
  summarize(count = n(),
            area = sum(area_sqft) / 1000000) %>%
  group_by(year, state, maintainer_class) %>%
  mutate(count_percent = count / sum(count, na.rm = TRUE),
         area_percent = area / sum(area, na.rm = TRUE)) %>%
  ungroup()

condition_deficient_main <- condition_count_main %>% filter(condition == "Poor")

#count bridges in deficient condition, by maintainer type
condition_count_all_main <- bridges_region %>%
  filter(!is.na(condition)) %>%
  group_by(year, state, maintainer_code, condition) %>%
  summarize(maintainer_class = first(maintainer_class),
            count = n(),
            area = sum(area_sqft) / 1000000) %>%
  group_by(year, state, maintainer_code) %>%
  mutate(count_percent = count / sum(count, na.rm = TRUE),
         area_percent = area / sum(area, na.rm = TRUE)) %>%
  ungroup()

condition_deficient_all_main <- condition_count_all_main %>% filter(condition == "Poor")

#count bridges in deficient condition, by owner class
condition_count_owner <- bridges_region %>%
  filter(!is.na(condition)) %>%
  group_by(year, state, owner_class, condition) %>%
  summarize(count = n(),
            area = sum(area_sqft) / 1000000) %>%
  group_by(year, state, owner_class, .drop = FALSE) %>%
  mutate(count_percent = count / sum(count, na.rm = TRUE),
         area_percent = area / sum(area, na.rm = TRUE)) %>%
  ungroup()

condition_deficient_owner <- condition_count_owner %>% filter(condition == "Poor")

#count bridges in deficient condition, by owner type
condition_count_all_owner <- bridges_region %>%
  filter(!is.na(condition)) %>%
  group_by(year, state, owner_code, condition) %>%
  summarize(owner_class = first(owner_class),
            count = n(),
            area = sum(area_sqft) / 1000000) %>%
  group_by(year, state, owner_code) %>%
  mutate(count_percent = count / sum(count, na.rm = TRUE),
         area_percent = area / sum(area, na.rm = TRUE)) %>%
  ungroup()

condition_deficient_owner <- condition_count_owner %>% filter(condition == "Poor")