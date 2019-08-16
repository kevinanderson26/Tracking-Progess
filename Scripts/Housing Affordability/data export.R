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
source("Scripts/Housing Affordability/data processing.R")

#graph 1: overall ratio of households paying more than 35% for housing
ovr_ratio_county <- ratio_all %>% filter(geo_type != "planning area") %>%
  select(year, geography, ratio) %>% spread(geography, ratio) %>%
  select(year, DVRPC, 'Pennsylvania Suburbs', 'New Jersey Suburbs', 'Philadelphia Subregion', everything())

ovr_ratio_PA <- ratio_all %>% filter(geo_type == "planning area")%>%
  filter(year %in% c(2012, 2017)) %>%
  select(year, geography, ratio) %>% spread(geography, ratio)

housing_afford_graph_1 <- full_join(ovr_ratio_county, ovr_ratio_PA, by = "year") %>% round(3)

rm(ovr_ratio_county, ovr_ratio_PA)

#graph 2: total number of households by ownership and affordability 

scaling_factor <- 1000
rounding_factor <- 1

class_households_region <- afford_county %>% group_by(year, class) %>%
  summarize(geography = "DVRPC", households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  ungroup()

class_households_subregion <- afford_county %>% group_by(year, subregion, class) %>%
  summarize(households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  rename(geography = subregion) %>%
  ungroup()

class_households_county <- afford_county %>% group_by(year, co_name, class) %>%
  summarize(households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  rename(geography = co_name) %>%
  ungroup()

class_households_planningarea <- afford_munic %>% group_by(year, planning_area, class) %>%
  summarize(households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  filter(year %in% c(2012, 2017)) %>%
  rename(geography = planning_area) %>%
  ungroup()

housing_afford_graph_2 <- bind_rows(class_households_region, class_households_subregion, 
                                    class_households_county, class_households_planningarea) %>% 
                    mutate(class_geo = paste(geography, class, sep = "- ")) %>%
                    select(year, class_geo, households) %>%
                    spread(class_geo, households) %>%
                    select(year, 'DVRPC- Not Computed- Own', 'DVRPC- Not Computed- Rent', 
                           'DVRPC- Below 35%- Own', 'DVRPC- Below 35%- Rent',
                           'DVRPC- Above 35%- Own', 'DVRPC- Above 35%- Rent',
                           'Pennsylvania Suburbs- Not Computed- Own', 'Pennsylvania Suburbs- Not Computed- Rent', 
                           'Pennsylvania Suburbs- Below 35%- Own', 'Pennsylvania Suburbs- Below 35%- Rent',
                           'Pennsylvania Suburbs- Above 35%- Own', 'Pennsylvania Suburbs- Above 35%- Rent',
                           'New Jersey Suburbs- Not Computed- Own', 'New Jersey Suburbs- Not Computed- Rent', 
                           'New Jersey Suburbs- Below 35%- Own', 'New Jersey Suburbs- Below 35%- Rent',
                           'New Jersey Suburbs- Above 35%- Own', 'New Jersey Suburbs- Above 35%- Rent',
                           'Philadelphia Subregion- Not Computed- Own', 'Philadelphia Subregion- Not Computed- Rent', 
                           'Philadelphia Subregion- Below 35%- Own', 'Philadelphia Subregion- Below 35%- Rent',
                           'Philadelphia Subregion- Above 35%- Own', 'Philadelphia Subregion- Above 35%- Rent',
                           'Core City- Not Computed- Own', 'Core City- Not Computed- Rent', 
                           'Core City- Below 35%- Own', 'Core City- Below 35%- Rent',
                           'Core City- Above 35%- Own', 'Core City- Above 35%- Rent',
                           'Developed Community- Not Computed- Own', 'Developed Community- Not Computed- Rent', 
                           'Developed Community- Below 35%- Own', 'Developed Community- Below 35%- Rent',
                           'Developed Community- Above 35%- Own', 'Developed Community- Above 35%- Rent',
                           'Growing Suburb- Not Computed- Own', 'Growing Suburb- Not Computed- Rent', 
                           'Growing Suburb- Below 35%- Own', 'Growing Suburb- Below 35%- Rent',
                           'Growing Suburb- Above 35%- Own', 'Growing Suburb- Above 35%- Rent',
                           'Rural Area- Not Computed- Own', 'Rural Area- Not Computed- Rent', 
                           'Rural Area- Below 35%- Own', 'Rural Area- Below 35%- Rent',
                           'Rural Area- Above 35%- Own', 'Rural Area- Above 35%- Rent',
                           'Bucks County- Not Computed- Own', 'Bucks County- Not Computed- Rent', 
                           'Bucks County- Below 35%- Own', 'Bucks County- Below 35%- Rent',
                           'Bucks County- Above 35%- Own', 'Bucks County- Above 35%- Rent',
                           'Chester County- Not Computed- Own', 'Chester County- Not Computed- Rent', 
                           'Chester County- Below 35%- Own', 'Chester County- Below 35%- Rent',
                           'Chester County- Above 35%- Own', 'Chester County- Above 35%- Rent',
                           'Delaware County- Not Computed- Own', 'Delaware County- Not Computed- Rent', 
                           'Delaware County- Below 35%- Own', 'Delaware County- Below 35%- Rent',
                           'Delaware County- Above 35%- Own', 'Delaware County- Above 35%- Rent',
                           'Montgomery County- Not Computed- Own', 'Montgomery County- Not Computed- Rent', 
                           'Montgomery County- Below 35%- Own', 'Montgomery County- Below 35%- Rent',
                           'Montgomery County- Above 35%- Own', 'Montgomery County- Above 35%- Rent',
                           'Philadelphia County- Not Computed- Own', 'Philadelphia County- Not Computed- Rent', 
                           'Philadelphia County- Below 35%- Own', 'Philadelphia County- Below 35%- Rent',
                           'Philadelphia County- Above 35%- Own', 'Philadelphia County- Above 35%- Rent',
                           'Burlington County- Not Computed- Own', 'Burlington County- Not Computed- Rent', 
                           'Burlington County- Below 35%- Own', 'Burlington County- Below 35%- Rent',
                           'Burlington County- Above 35%- Own', 'Burlington County- Above 35%- Rent',
                           'Camden County- Not Computed- Own', 'Camden County- Not Computed- Rent', 
                           'Camden County- Below 35%- Own', 'Camden County- Below 35%- Rent',
                           'Camden County- Above 35%- Own', 'Camden County- Above 35%- Rent',
                           'Gloucester County- Not Computed- Own', 'Gloucester County- Not Computed- Rent', 
                           'Gloucester County- Below 35%- Own', 'Gloucester County- Below 35%- Rent',
                           'Gloucester County- Above 35%- Own', 'Gloucester County- Above 35%- Rent',
                           'Mercer County- Not Computed- Own', 'Mercer County- Not Computed- Rent', 
                           'Mercer County- Below 35%- Own', 'Mercer County- Below 35%- Rent',
                           'Mercer County- Above 35%- Own', 'Mercer County- Above 35%- Rent')

rm(class_households_region, class_households_subregion, 
   class_households_county, class_households_planningarea)

#graph 3: income brackets by geography
#scaling and rounding factors defined in graph 2
brackets_region <- afford_county %>% group_by(year, cost_bracket) %>%
  summarize(geography = "DVRPC", households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  ungroup()

brackets_subregion <- afford_county %>% group_by(year, subregion, cost_bracket) %>%
  summarize(households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  rename(geography = subregion) %>%
  ungroup()

brackets_county <- afford_county %>% group_by(year, co_name, cost_bracket) %>%
  summarize(households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  rename(geography = co_name) %>%
  ungroup()

brackets_planningarea <- afford_munic %>% group_by(year, planning_area, cost_bracket) %>%
  summarize(households = sum(n)) %>%
  mutate(households = households / scaling_factor,
         households = round(households, rounding_factor)) %>%
  filter(year %in% c(2012, 2017)) %>%
  rename(geography = planning_area) %>%
  ungroup()

brackets_temp1 <- bind_rows(brackets_region, brackets_subregion, 
                            brackets_county, brackets_planningarea) %>% 
                  mutate(geo_bracket = paste(geography, cost_bracket, sep = "- ")) %>%
                  select(year, geo_bracket, households) %>%
                  spread(geo_bracket, households) %>%
                  select(year, 'DVRPC- Not Computed', 'DVRPC- 0.0-9.9%', 'DVRPC- 10.0-14.9%', 'DVRPC- 15.0-19.9%', 'DVRPC- 20.0-24.9%', 'DVRPC- 25.0-29.9%', 'DVRPC- 30.0-34.9%', 'DVRPC- 35.0-39.9%', 'DVRPC- 40.0-49.9%', 'DVRPC- 50%+', 'Pennsylvania Suburbs- Not Computed', 'Pennsylvania Suburbs- 0.0-9.9%', 'Pennsylvania Suburbs- 10.0-14.9%', 'Pennsylvania Suburbs- 15.0-19.9%', 'Pennsylvania Suburbs- 20.0-24.9%', 'Pennsylvania Suburbs- 25.0-29.9%', 'Pennsylvania Suburbs- 30.0-34.9%', 'Pennsylvania Suburbs- 35.0-39.9%', 'Pennsylvania Suburbs- 40.0-49.9%', 'Pennsylvania Suburbs- 50%+', 'New Jersey Suburbs- Not Computed', 'New Jersey Suburbs- 0.0-9.9%', 'New Jersey Suburbs- 10.0-14.9%', 'New Jersey Suburbs- 15.0-19.9%', 'New Jersey Suburbs- 20.0-24.9%', 'New Jersey Suburbs- 25.0-29.9%', 'New Jersey Suburbs- 30.0-34.9%', 'New Jersey Suburbs- 35.0-39.9%', 'New Jersey Suburbs- 40.0-49.9%', 'New Jersey Suburbs- 50%+', 'Philadelphia Subregion- Not Computed', 'Philadelphia Subregion- 0.0-9.9%', 'Philadelphia Subregion- 10.0-14.9%', 'Philadelphia Subregion- 15.0-19.9%', 'Philadelphia Subregion- 20.0-24.9%', 'Philadelphia Subregion- 25.0-29.9%', 'Philadelphia Subregion- 30.0-34.9%', 'Philadelphia Subregion- 35.0-39.9%', 'Philadelphia Subregion- 40.0-49.9%', 'Philadelphia Subregion- 50%+', 'Core City- Not Computed', 'Core City- 0.0-9.9%', 'Core City- 10.0-14.9%', 'Core City- 15.0-19.9%', 'Core City- 20.0-24.9%', 'Core City- 25.0-29.9%', 'Core City- 30.0-34.9%', 'Core City- 35.0-39.9%', 'Core City- 40.0-49.9%', 'Core City- 50%+', 'Developed Community- Not Computed', 'Developed Community- 0.0-9.9%', 'Developed Community- 10.0-14.9%', 'Developed Community- 15.0-19.9%', 'Developed Community- 20.0-24.9%', 'Developed Community- 25.0-29.9%', 'Developed Community- 30.0-34.9%', 'Developed Community- 35.0-39.9%', 'Developed Community- 40.0-49.9%', 'Developed Community- 50%+', 'Growing Suburb- Not Computed', 'Growing Suburb- 0.0-9.9%', 'Growing Suburb- 10.0-14.9%', 'Growing Suburb- 15.0-19.9%', 'Growing Suburb- 20.0-24.9%', 'Growing Suburb- 25.0-29.9%', 'Growing Suburb- 30.0-34.9%', 'Growing Suburb- 35.0-39.9%', 'Growing Suburb- 40.0-49.9%', 'Growing Suburb- 50%+', 'Rural Area- Not Computed', 'Rural Area- 0.0-9.9%', 'Rural Area- 10.0-14.9%', 'Rural Area- 15.0-19.9%', 'Rural Area- 20.0-24.9%', 'Rural Area- 25.0-29.9%', 'Rural Area- 30.0-34.9%', 'Rural Area- 35.0-39.9%', 'Rural Area- 40.0-49.9%', 'Rural Area- 50%+')
                                 
brackets_temp2 <- bind_rows(brackets_region, brackets_subregion, 
                            brackets_county, brackets_planningarea) %>% 
                  mutate(geo_bracket = paste(geography, cost_bracket, sep = "- ")) %>%
                  select(year, geo_bracket, households) %>%
                  spread(geo_bracket, households) %>%
                  select('Bucks County- Not Computed', 'Bucks County- 0.0-9.9%', 'Bucks County- 10.0-14.9%', 'Bucks County- 15.0-19.9%', 'Bucks County- 20.0-24.9%', 'Bucks County- 25.0-29.9%', 'Bucks County- 30.0-34.9%', 'Bucks County- 35.0-39.9%', 'Bucks County- 40.0-49.9%', 'Bucks County- 50%+', 'Chester County- Not Computed', 'Chester County- 0.0-9.9%', 'Chester County- 10.0-14.9%', 'Chester County- 15.0-19.9%', 'Chester County- 20.0-24.9%', 'Chester County- 25.0-29.9%', 'Chester County- 30.0-34.9%', 'Chester County- 35.0-39.9%', 'Chester County- 40.0-49.9%', 'Chester County- 50%+', 'Delaware County- Not Computed', 'Delaware County- 0.0-9.9%', 'Delaware County- 10.0-14.9%', 'Delaware County- 15.0-19.9%', 'Delaware County- 20.0-24.9%', 'Delaware County- 25.0-29.9%', 'Delaware County- 30.0-34.9%', 'Delaware County- 35.0-39.9%', 'Delaware County- 40.0-49.9%', 'Delaware County- 50%+', 'Montgomery County- Not Computed', 'Montgomery County- 0.0-9.9%', 'Montgomery County- 10.0-14.9%', 'Montgomery County- 15.0-19.9%', 'Montgomery County- 20.0-24.9%', 'Montgomery County- 25.0-29.9%', 'Montgomery County- 30.0-34.9%', 'Montgomery County- 35.0-39.9%', 'Montgomery County- 40.0-49.9%', 'Montgomery County- 50%+', 'Philadelphia County- Not Computed', 'Philadelphia County- 0.0-9.9%', 'Philadelphia County- 10.0-14.9%', 'Philadelphia County- 15.0-19.9%', 'Philadelphia County- 20.0-24.9%', 'Philadelphia County- 25.0-29.9%', 'Philadelphia County- 30.0-34.9%', 'Philadelphia County- 35.0-39.9%', 'Philadelphia County- 40.0-49.9%', 'Philadelphia County- 50%+', 'Burlington County- Not Computed', 'Burlington County- 0.0-9.9%', 'Burlington County- 10.0-14.9%', 'Burlington County- 15.0-19.9%', 'Burlington County- 20.0-24.9%', 'Burlington County- 25.0-29.9%', 'Burlington County- 30.0-34.9%', 'Burlington County- 35.0-39.9%', 'Burlington County- 40.0-49.9%', 'Burlington County- 50%+', 'Camden County- Not Computed', 'Camden County- 0.0-9.9%', 'Camden County- 10.0-14.9%', 'Camden County- 15.0-19.9%', 'Camden County- 20.0-24.9%', 'Camden County- 25.0-29.9%', 'Camden County- 30.0-34.9%', 'Camden County- 35.0-39.9%', 'Camden County- 40.0-49.9%', 'Camden County- 50%+', 'Gloucester County- Not Computed', 'Gloucester County- 0.0-9.9%', 'Gloucester County- 10.0-14.9%', 'Gloucester County- 15.0-19.9%', 'Gloucester County- 20.0-24.9%', 'Gloucester County- 25.0-29.9%', 'Gloucester County- 30.0-34.9%', 'Gloucester County- 35.0-39.9%', 'Gloucester County- 40.0-49.9%', 'Gloucester County- 50%+', 'Mercer County- Not Computed', 'Mercer County- 0.0-9.9%', 'Mercer County- 10.0-14.9%', 'Mercer County- 15.0-19.9%', 'Mercer County- 20.0-24.9%', 'Mercer County- 25.0-29.9%', 'Mercer County- 30.0-34.9%', 'Mercer County- 35.0-39.9%', 'Mercer County- 40.0-49.9%', 'Mercer County- 50%+')

housing_afford_graph_3 <- bind_cols(brackets_temp1, brackets_temp2)

rm(brackets_region, brackets_subregion, brackets_county, brackets_planningarea, brackets_temp1, brackets_temp2)

#export data
write_csv(housing_afford_graph_1, "Outputs/Housing Affordability/housing_afford_graph_1.csv")
write_csv(housing_afford_graph_2, "Outputs/Housing Affordability/housing_afford_graph_2.csv")
write_csv(housing_afford_graph_3, "Outputs/Housing Affordability/housing_afford_graph_3.csv")
  