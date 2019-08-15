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

#import processed data
source("Scripts/Bridge Conditions/2) Data Processing.R")

#Graph 1A: percent deficient bridge deck area by maintenance responsibility for a single geography
graph_1A_region_all_main <- bridges_region %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, percent_area) %>%
  rename('DVRPC- All' = percent_area)

graph_1A_state_all_main <- bridges_region %>%
  filter(state == "Pennsylvania") %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, percent_area) %>%
  rename('Pennsylvania All Counties- All' = percent_area)

graph_1A_subregion_all_main <- bridges_region %>%
  group_by(year, subregion, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year, subregion) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor" & subregion != "Philadelphia") %>%
  mutate(geo_main = paste(subregion, "All", sep = "- ")) %>%
  select(year, geo_main, percent_area) %>%
  spread(geo_main, percent_area, fill = 0) %>%
  select('year', 'Pennsylvania Suburbs- All', 'New Jersey Suburbs- All')

graph_1A_county_all_main <- bridges_region %>%
  group_by(year, county, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year, county) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  mutate(geo_main = paste(county, "All", sep = "- ")) %>%
  select(year, geo_main, percent_area) %>%
  spread(geo_main, percent_area, fill = 0)

graph_1A_region <- bridges_region %>%
  group_by(year, maintainer_class, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year, maintainer_class) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, maintainer_class, percent_area) %>%
  mutate(maintainer_class = paste("DVRPC-", maintainer_class, sep = " ")) %>%
  spread(maintainer_class, percent_area) %>%
  select('year', 'DVRPC- State', 'DVRPC- Local', 'DVRPC- Other')

graph_1A_state <- bridges_region %>%
  filter(state == "Pennsylvania") %>%
  group_by(year, maintainer_class, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year, maintainer_class) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, maintainer_class, percent_area) %>%
  mutate(maintainer_class = paste("Pennsylvania All Counties-", maintainer_class, sep = " ")) %>%
  spread(maintainer_class, percent_area) %>%
  select('year', 'Pennsylvania All Counties- State', 
         'Pennsylvania All Counties- Local', 'Pennsylvania All Counties- Other')

graph_1A_subregion <- bridges_region %>%
  group_by(year, maintainer_class, subregion, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year, maintainer_class, subregion) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor" & subregion != "Philadelphia") %>%
  mutate(geo_main = paste(subregion, maintainer_class, sep = "- ")) %>%
  select(year, geo_main, percent_area) %>%
  spread(geo_main, percent_area, fill = 0) %>%
  select('year', 
         'Pennsylvania Suburbs- State', 'Pennsylvania Suburbs- Local', 'Pennsylvania Suburbs- Other',
         'New Jersey Suburbs- State', 'New Jersey Suburbs- Local', 'New Jersey Suburbs- Other')

graph_1A_county <- bridges_region %>%
  group_by(year, maintainer_class, county, condition) %>%
  summarize(area = sum(area_sqft_adj)) %>%
  group_by(year, maintainer_class, county) %>%
  mutate(percent_area = area / sum(area, na.rm = TRUE),
         percent_area = round(percent_area, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  mutate(geo_main = paste(county, maintainer_class, sep = "- ")) %>%
  select(year, geo_main, percent_area) %>%
  spread(geo_main, percent_area, fill = 0) %>%
  mutate('Gloucester- Other' = 0,
         'Mercer- Other' = 0) %>%
  select('year', 
         'Bucks- State', 'Bucks- Local', 'Bucks- Other', 
         'Burlington- State', 'Burlington- Local', 'Burlington- Other', 
         'Camden- State', 'Camden- Local', 'Camden- Other', 
         'Chester- State', 'Chester- Local', 'Chester- Other', 
         'Delaware- State', 'Delaware- Local', 'Delaware- Other', 
         'Gloucester- State', 'Gloucester- Local', 'Gloucester- Other', 
         'Mercer- State', 'Mercer- Local', 'Mercer- Other',
         'Montgomery- State', 'Montgomery- Local', 'Montgomery- Other', 
         'Philadelphia- State', 'Philadelphia- Local', 'Philadelphia- Other')

graph_1A <- left_join(graph_1A_region, graph_1A_state, by = "year") %>%
  left_join(graph_1A_subregion, by = "year") %>%
  left_join(graph_1A_county, by = "year") %>%
  left_join(graph_1A_region_all_main, by = "year") %>%
  left_join(graph_1A_state_all_main, by = "year") %>%
  left_join(graph_1A_subregion_all_main, by = "year") %>%
  left_join(graph_1A_county_all_main, by = "year") %>%
  select('year',
          'DVRPC- All', 'DVRPC- State', 'DVRPC- Local', 'DVRPC- Other',
          'Pennsylvania All Counties- All', 'Pennsylvania All Counties- State',
          'Pennsylvania All Counties- Local', 'Pennsylvania All Counties- Other',
          'Pennsylvania Suburbs- All', 'Pennsylvania Suburbs- State',
          'Pennsylvania Suburbs- Local', 'Pennsylvania Suburbs- Other',
          'New Jersey Suburbs- All', 'New Jersey Suburbs- State',
          'New Jersey Suburbs- Local', 'New Jersey Suburbs- Other',
          'Bucks- All', 'Bucks- State', 'Bucks- Local', 'Bucks- Other',
          'Burlington- All', 'Burlington- State', 'Burlington- Local', 'Burlington- Other',
          'Camden- All', 'Camden- State', 'Camden- Local', 'Camden- Other',
          'Chester- All', 'Chester- State', 'Chester- Local', 'Chester- Other',
          'Delaware- All', 'Delaware- State', 'Delaware- Local', 'Delaware- Other',
          'Gloucester- All', 'Gloucester- State', 'Gloucester- Local', 'Gloucester- Other',
          'Mercer- All', 'Mercer- State', 'Mercer- Local', 'Mercer- Other',
          'Montgomery- All', 'Montgomery- State', 'Montgomery- Local', 'Montgomery- Other',
          'Philadelphia- All', 'Philadelphia- State', 'Philadelphia- Local', 'Philadelphia- Other')

rm(graph_1A_region, graph_1A_state, graph_1A_subregion, graph_1A_county,
   graph_1A_region_all_main, graph_1A_state_all_main, 
   graph_1A_subregion_all_main, graph_1A_county_all_main)

#Graph 1B: percent of deficient bridges by maintenance responsibility

graph_1B_region_all_main <- bridges_region %>%
  group_by(year, condition) %>%
  summarize(count = n()) %>%
  group_by(year) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, percent_count) %>%
  rename('DVRPC- All' = percent_count)


graph_1B_state_all_main <- bridges_region %>%
  filter(state == "Pennsylvania") %>%
  group_by(year, condition) %>%
  summarize(count = n()) %>%
  group_by(year) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, percent_count) %>%
  rename('Pennsylvania All Counties- All' = percent_count)

graph_1B_subregion_all_main <- bridges_region %>%
  group_by(year, subregion, condition) %>%
  summarize(count = n()) %>%
  group_by(year, subregion) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor" & subregion != "Philadelphia") %>%
  mutate(geo_main = paste(subregion, "All", sep = "- ")) %>%
  select(year, geo_main, percent_count) %>%
  spread(geo_main, percent_count, fill = 0) %>%
  select('year', 
         'Pennsylvania Suburbs- All', 'New Jersey Suburbs- All')

graph_1B_county_all_main <- bridges_region %>%
  group_by(year, county, condition) %>%
  summarize(count = n()) %>%
  group_by(year, county) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 3)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  mutate(geo_main = paste(county, "All", sep = "- ")) %>%
  select(year, geo_main, percent_count) %>%
  spread(geo_main, percent_count, fill = 0) %>%
  select('year', 
         'Bucks- All', 
         'Burlington- All', 
         'Camden- All',
         'Chester- All',  
         'Delaware- All',
         'Gloucester- All',
         'Mercer- All',
         'Montgomery- All',
         'Philadelphia- All')

graph_1B_region <- bridges_region %>%
  group_by(year, maintainer_class, condition) %>%
  summarize(count = n()) %>%
  group_by(year, maintainer_class) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 4)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, maintainer_class, percent_count) %>%
  mutate(maintainer_class = paste("DVRPC-", maintainer_class, sep = " ")) %>%
  spread(maintainer_class, percent_count) %>%
  select('year', 'DVRPC- State', 'DVRPC- Local', 'DVRPC- Other')


graph_1B_state <- bridges_region %>%
  filter(state == "Pennsylvania") %>%
  group_by(year, maintainer_class, condition) %>%
  summarize(count = n()) %>%
  group_by(year, maintainer_class) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 4)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  select(year, maintainer_class, percent_count) %>%
  mutate(maintainer_class = paste("Pennsylvania All Counties-", maintainer_class, sep = " ")) %>%
  spread(maintainer_class, percent_count) %>%
  select('year', 'Pennsylvania All Counties- State', 
         'Pennsylvania All Counties- Local', 'Pennsylvania All Counties- Other')

graph_1B_subregion <- bridges_region %>%
  group_by(year, maintainer_class, subregion, condition) %>%
  summarize(count = n()) %>%
  group_by(year, maintainer_class, subregion) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 4)) %>%
  ungroup() %>%
  filter(condition == "Poor" & subregion != "Philadelphia") %>%
  mutate(geo_main = paste(subregion, maintainer_class, sep = "- ")) %>%
  select(year, geo_main, percent_count) %>%
  spread(geo_main, percent_count, fill = 0) %>%
  select('year', 
         'Pennsylvania Suburbs- State', 'Pennsylvania Suburbs- Local', 'Pennsylvania Suburbs- Other',
         'New Jersey Suburbs- State', 'New Jersey Suburbs- Local', 'New Jersey Suburbs- Other')

graph_1B_county <- bridges_region %>%
  group_by(year, maintainer_class, county, condition) %>%
  summarize(count = n()) %>%
  group_by(year, maintainer_class, county) %>%
  mutate(percent_count = count / sum(count, na.rm = TRUE),
         percent_count = round(percent_count, 4)) %>%
  ungroup() %>%
  filter(condition == "Poor") %>%
  mutate(geo_main = paste(county, maintainer_class, sep = "- ")) %>%
  select(year, geo_main, percent_count) %>%
  spread(geo_main, percent_count, fill = 0) %>%
  mutate('Gloucester- Other' = 0,
         'Mercer- Other' = 0) %>%
  select('year', 
         'Bucks- State', 'Bucks- Local', 'Bucks- Other', 
         'Burlington- State', 'Burlington- Local', 'Burlington- Other', 
         'Camden- State', 'Camden- Local', 'Camden- Other', 
         'Chester- State', 'Chester- Local', 'Chester- Other', 
         'Delaware- State', 'Delaware- Local', 'Delaware- Other', 
         'Gloucester- State', 'Gloucester- Local', 'Gloucester- Other', 
         'Mercer- State', 'Mercer- Local', 'Mercer- Other',
         'Montgomery- State', 'Montgomery- Local', 'Montgomery- Other', 
         'Philadelphia- State', 'Philadelphia- Local', 'Philadelphia- Other')

graph_1B <- left_join(graph_1B_region, graph_1B_state, by = "year") %>%
  left_join(graph_1B_subregion, by = "year") %>%
  left_join(graph_1B_county, by = "year") %>%
  left_join(graph_1B_region_all_main, by = "year") %>%
  left_join(graph_1B_state_all_main, by = "year") %>%
  left_join(graph_1B_subregion_all_main, by = "year") %>%
  left_join(graph_1B_county_all_main, by = "year") %>%
  select('year',
         'DVRPC- All', 'DVRPC- State', 'DVRPC- Local', 'DVRPC- Other',
         'Pennsylvania All Counties- All', 'Pennsylvania All Counties- State',
         'Pennsylvania All Counties- Local', 'Pennsylvania All Counties- Other',
         'Pennsylvania Suburbs- All', 'Pennsylvania Suburbs- State',
         'Pennsylvania Suburbs- Local', 'Pennsylvania Suburbs- Other',
         'New Jersey Suburbs- All', 'New Jersey Suburbs- State',
         'New Jersey Suburbs- Local', 'New Jersey Suburbs- Other',
         'Bucks- All', 'Bucks- State', 'Bucks- Local', 'Bucks- Other',
         'Burlington- All', 'Burlington- State', 'Burlington- Local', 'Burlington- Other',
         'Camden- All', 'Camden- State', 'Camden- Local', 'Camden- Other',
         'Chester- All', 'Chester- State', 'Chester- Local', 'Chester- Other',
         'Delaware- All', 'Delaware- State', 'Delaware- Local', 'Delaware- Other',
         'Gloucester- All', 'Gloucester- State', 'Gloucester- Local', 'Gloucester- Other',
         'Mercer- All', 'Mercer- State', 'Mercer- Local', 'Mercer- Other',
         'Montgomery- All', 'Montgomery- State', 'Montgomery- Local', 'Montgomery- Other',
         'Philadelphia- All', 'Philadelphia- State', 'Philadelphia- Local', 'Philadelphia- Other')

rm(graph_1B_region, graph_1B_state, graph_1B_subregion, graph_1B_county,
   graph_1B_region_all_main, graph_1B_state_all_main, 
   graph_1B_subregion_all_main, graph_1B_county_all_main)

#Graph 2A: total deficient bridge deck area by maintenance responsibility for a single geography

area_adjustment <- 1000
area_round <- 1

graph_2A_region <- bridges_region %>%
  filter(condition == "Poor") %>%
  mutate(maintainer_class = paste("DVRPC-", maintainer_class, sep = " ")) %>%
  group_by(year, maintainer_class) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(maintainer_class, area) %>%
  select('year', 'DVRPC- State', 'DVRPC- Local', 'DVRPC- Other')

graph_2A_state <- bridges_region %>%
  filter(state == "Pennsylvania" & condition == "Poor") %>%
  mutate(maintainer_class = paste("Pennsylvania All Counties-", maintainer_class, sep = " ")) %>%
  group_by(year, maintainer_class) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(maintainer_class, area) %>%
  select('year', 'Pennsylvania All Counties- State', 
         'Pennsylvania All Counties- Local', 'Pennsylvania All Counties- Other')

graph_2A_subregion <- bridges_region %>%
  filter(condition == "Poor" & subregion != "Philadelphia") %>%
  mutate(geo_main = paste(subregion, maintainer_class, sep = "- ")) %>%
  group_by(year, geo_main) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(geo_main, area, fill = 0) %>%
  select('year', 
         'Pennsylvania Suburbs- State', 'Pennsylvania Suburbs- Local', 'Pennsylvania Suburbs- Other',
         'New Jersey Suburbs- State', 'New Jersey Suburbs- Local', 'New Jersey Suburbs- Other')

graph_2A_county <- bridges_region %>%
  filter(condition == "Poor") %>%
  mutate(geo_main = paste(county, maintainer_class, sep = "- ")) %>%
  group_by(year, geo_main) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(geo_main, area, fill = 0) %>%
  mutate('Gloucester- Other' = 0,
         'Mercer- Other' = 0) %>%
  select('year', 
         'Bucks- State', 'Bucks- Local', 'Bucks- Other', 
         'Burlington- State', 'Burlington- Local', 'Burlington- Other', 
         'Camden- State', 'Camden- Local', 'Camden- Other', 
         'Chester- State', 'Chester- Local', 'Chester- Other', 
         'Delaware- State', 'Delaware- Local', 'Delaware- Other', 
         'Gloucester- State', 'Gloucester- Local', 'Gloucester- Other', 
         'Mercer- Local', 'Mercer- State', 'Mercer- Other',
         'Montgomery- State', 'Montgomery- Local', 'Montgomery- Other', 
         'Philadelphia- State', 'Philadelphia- Local', 'Philadelphia- Other')

graph_2A <- left_join(graph_2A_region, graph_2A_state, by = "year") %>%
  left_join(graph_2A_subregion, by = "year") %>%
  left_join(graph_2A_county, by = "year")

rm(graph_2A_region, graph_2A_state, graph_2A_subregion, graph_2A_county)

#Graph 2B: total number of deficient bridges by maintenance responsibility

graph_2B_region <- bridges_region %>%
  filter(condition == "Poor") %>%
  mutate(maintainer_class = paste("DVRPC-", maintainer_class, sep = " ")) %>%
  group_by(year, maintainer_class) %>%
  summarize(count = n()) %>%
  spread(maintainer_class, count) %>%
  select('year', 
         'DVRPC- State', 'DVRPC- Local', 'DVRPC- Other')

graph_2B_state <- bridges_region %>%
  filter(condition == "Poor" & state == "Pennsylvania") %>%
  mutate(maintainer_class = paste("Pennsylvania All Counties-", maintainer_class, sep = " ")) %>%
  group_by(year, maintainer_class) %>%
  summarize(count = n()) %>%
  spread(maintainer_class, count) %>%
  select('year', 'Pennsylvania All Counties- State', 
         'Pennsylvania All Counties- Local', 'Pennsylvania All Counties- Other')


graph_2B_subregion <- bridges_region %>%
  filter(condition == "Poor" & subregion != "Philadelphia") %>%
  mutate(geo_main = paste(subregion, maintainer_class, sep = "- ")) %>%
  group_by(year, geo_main) %>%
  summarize(count = n()) %>%
  spread(geo_main, count, fill = 0) %>%
  select('year', 
         'Pennsylvania Suburbs- State', 'Pennsylvania Suburbs- Local', 'Pennsylvania Suburbs- Other',
         'New Jersey Suburbs- State', 'New Jersey Suburbs- Local', 'New Jersey Suburbs- Other')

graph_2B_county <- bridges_region %>%
  filter(condition == "Poor") %>%
  mutate(geo_main = paste(county, maintainer_class, sep = "- ")) %>%
  group_by(year, geo_main) %>%
  summarize(count = n()) %>%
  spread(geo_main, count, fill = 0) %>%
  mutate("Gloucester- Other" = 0,
         "Mercer- Other" = 0) %>%
  select('year', 
         'Bucks- State', 'Bucks- Local', 'Bucks- Other', 
         'Burlington- State', 'Burlington- Local', 'Burlington- Other', 
         'Camden- State', 'Camden- Local', 'Camden- Other', 
         'Chester- State', 'Chester- Local', 'Chester- Other', 
         'Delaware- State', 'Delaware- Local', 'Delaware- Other', 
         'Gloucester- State', 'Gloucester- Local', 'Gloucester- Other', 
         'Mercer- Local', 'Mercer- State', 'Mercer- Other',
         'Montgomery- State', 'Montgomery- Local', 'Montgomery- Other', 
         'Philadelphia- State', 'Philadelphia- Local', 'Philadelphia- Other')

graph_2B <- left_join(graph_2B_region, graph_2B_state, by = "year") %>%
  left_join(graph_2B_subregion, by = "year") %>%
  left_join(graph_2B_county, by = "year")

rm(graph_2B_region, graph_2B_state, graph_2B_subregion, graph_2B_county)

#Graph 3A: total State-maintained bridge deck area by condition for a single geography
#area adjustment factors are defined in Graph 2A
graph_3A_region <- bridges_region %>%
  filter(maintainer_class == "State") %>%
  mutate(condition = paste("DVRPC", condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  select(year, 'DVRPC- NA', 'DVRPC- Poor', 'DVRPC- Fair', 'DVRPC- Good')

graph_3A_state <- bridges_region %>%
  filter(maintainer_class == "State" & state == "Pennsylvania") %>%
  mutate(condition = paste("Pennsylvania All Counties", condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  select(year, 'Pennsylvania All Counties- NA', 'Pennsylvania All Counties- Poor', 
         'Pennsylvania All Counties- Fair', 'Pennsylvania All Counties- Good')

graph_3A_subregion <- bridges_region %>%
  filter(maintainer_class == "State" & county != "Philadelphia") %>%
  mutate(condition = paste(subregion, condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj / area_adjustment)) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  mutate('New Jersey Suburbs- NA' = 0) %>%
  select(year, 
         'Pennsylvania Suburbs- NA', 'Pennsylvania Suburbs- Poor', 
         'Pennsylvania Suburbs- Fair', 'Pennsylvania Suburbs- Good',
         'New Jersey Suburbs- NA', 'New Jersey Suburbs- Poor', 
         'New Jersey Suburbs- Fair', 'New Jersey Suburbs- Good')
  
graph_3A_county <- bridges_region %>%
  filter(maintainer_class == "State") %>%
  mutate(condition = paste(county, condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  mutate('Burlington- NA' = 0,
         'Camden- NA' = 0,
         'Gloucester- NA' = 0,
         'Mercer- NA' = 0) %>%
  select(year, 'Bucks- NA', 'Bucks- Poor', 'Bucks- Fair', 'Bucks- Good',
         'Burlington- NA', 'Burlington- Poor', 'Burlington- Fair', 'Burlington- Good',
         'Camden- NA', 'Camden- Poor', 'Camden- Fair', 'Camden- Good',
         'Chester- NA', 'Chester- Poor', 'Chester- Fair', 'Chester- Good',
         'Delaware- NA', 'Delaware- Poor', 'Delaware- Fair', 'Delaware- Good',
         'Gloucester- NA', 'Gloucester- Poor', 'Gloucester- Fair', 'Gloucester- Good',
         'Mercer- NA', 'Mercer- Poor', 'Mercer- Fair', 'Mercer- Good',
         'Montgomery- NA', 'Montgomery- Poor', 'Montgomery- Fair', 'Montgomery- Good',
         'Philadelphia- NA', 'Philadelphia- Poor', 'Philadelphia- Fair', 'Philadelphia- Good')

graph_3A <- left_join(graph_3A_region, graph_3A_state, by = "year") %>%
  left_join(graph_3A_subregion, by = "year") %>%
  left_join(graph_3A_county, by = "year")

rm(graph_3A_region, graph_3A_state, graph_3A_subregion, graph_3A_county)


#Graph 3B: total Local-maintained bridge deck area by condition for a single geography
graph_3B_region <- bridges_region %>%
  filter(maintainer_class == "Local") %>%
  mutate(condition = paste("DVRPC", condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  select(year, 'DVRPC- NA', 'DVRPC- Poor', 'DVRPC- Fair', 'DVRPC- Good')

graph_3B_state <- bridges_region %>%
  filter(maintainer_class == "Local" & state == "Pennsylvania") %>%
  mutate(condition = paste("Pennsylvania All Counties", condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  select(year, 'Pennsylvania All Counties- NA', 'Pennsylvania All Counties- Poor', 
         'Pennsylvania All Counties- Fair', 'Pennsylvania All Counties- Good')

graph_3B_subregion <- bridges_region %>%
  filter(maintainer_class == "Local" & county != "Philadelphia") %>%
  mutate(condition = paste(subregion, condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  mutate('New Jersey Suburbs- NA' = 0) %>%
  select(year, 
         'Pennsylvania Suburbs- NA', 'Pennsylvania Suburbs- Poor', 
         'Pennsylvania Suburbs- Fair', 'Pennsylvania Suburbs- Good',
         'New Jersey Suburbs- NA', 'New Jersey Suburbs- Poor', 
         'New Jersey Suburbs- Fair', 'New Jersey Suburbs- Good')

graph_3B_county <- bridges_region %>%
  filter(maintainer_class == "Local") %>%
  mutate(condition = paste(county, condition, sep = "- ")) %>%
  group_by(year, condition) %>%
  summarize(area = sum(area_sqft_adj) / area_adjustment) %>%
  mutate(area = round(area, area_round)) %>%
  spread(condition, area, fill = 0) %>%
  mutate('Burlington- NA' = 0,
         'Camden- NA' = 0,
         'Delaware- NA' = 0,
         'Gloucester- NA' = 0,
         'Mercer- NA' = 0,
         'Montgomery- NA' = 0,
         'Philadelphia- NA' = 0) %>%
  select(year, 'Bucks- NA', 'Bucks- Poor', 'Bucks- Fair', 'Bucks- Good',
         'Burlington- NA', 'Burlington- Poor', 'Burlington- Fair', 'Burlington- Good',
         'Camden- NA', 'Camden- Poor', 'Camden- Fair', 'Camden- Good',
         'Chester- NA', 'Chester- Poor', 'Chester- Fair', 'Chester- Good',
         'Delaware- NA', 'Delaware- Poor', 'Delaware- Fair', 'Delaware- Good',
         'Gloucester- NA', 'Gloucester- Poor', 'Gloucester- Fair', 'Gloucester- Good',
         'Mercer- NA', 'Mercer- Poor', 'Mercer- Fair', 'Mercer- Good',
         'Montgomery- NA', 'Montgomery- Poor', 'Montgomery- Fair', 'Montgomery- Good',
         'Philadelphia- NA', 'Philadelphia- Poor', 'Philadelphia- Fair', 'Philadelphia- Good')

graph_3B <- left_join(graph_3B_region, graph_3B_state, by = "year") %>%
  left_join(graph_3B_subregion, by = "year")%>%
  left_join(graph_3B_county, by = "year") 

rm(graph_3B_region, graph_3B_state, graph_3B_subregion, graph_3B_county)

#write csv's for web
write_csv(graph_1A, "Outputs/Bridge Conditions/Bridge Conditions graph 1A.csv")
write_csv(graph_1B, "Outputs/Bridge Conditions/Bridge Conditions graph 1B.csv")
write_csv(graph_2A, "Outputs/Bridge Conditions/Bridge Conditions graph 2A.csv")
write_csv(graph_2B, "Outputs/Bridge Conditions/Bridge Conditions graph 2B.csv")
write_csv(graph_3A, "Outputs/Bridge Conditions/Bridge Conditions graph 3A.csv")
write_csv(graph_3B, "Outputs/Bridge Conditions/Bridge Conditions graph 3B.csv")