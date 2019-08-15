#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "sf", "lwgeom")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import data
source("Scripts/Land Consumption/Land Preservation/2) data processing.R")

#graph 3: total acres of public/private protected land
#3A: Total Acres of all protected land
#3B: Percent of all protected land
#3C: Total Acres of public protected land
#3D: Percent of all public protected land
#3E: Total Acres of private protected land
#3F: Percent of all private protected land

scaling_factor_acres <- 1000
rounding_factor_acres <- 1
rounding_factor_percent <- 3

temp_3_region_class <- pres_co_sum %>% group_by(year, pres_class) %>%
  summarize(acres = sum(acres),
            pres_percent = acres / sum(counties$area_acres)) %>%
  mutate(geography = "DVRPC",
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  select(year, geography, pres_class, acres, pres_percent)

temp_3_subregion_class <- pres_sub_sum %>% group_by(year, subregion, pres_class) %>%
  summarize(acres = sum(acres), 
            total_acres = first(area_total), 
            pres_percent = acres / total_acres) %>%
  mutate(geography = subregion,
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  ungroup() %>%
  select(year, geography, pres_class, acres, pres_percent) %>%
  filter(geography != "Philadelphia")

temp_3_pa_class <- pres_pa_sum %>% group_by(year, planning_area, pres_class) %>%
  summarize(acres = sum(acres), 
            total_acres = first(area_total), 
            pres_percent = acres / total_acres) %>%
  mutate(geography = planning_area,
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  ungroup() %>%
  select(year, geography, pres_class, acres, pres_percent)

temp_3_county_class <- pres_co_sum %>% group_by(year, county, pres_class) %>%
  summarize(acres = sum(acres), 
            total_acres = first(area_total), 
            pres_percent = acres / total_acres) %>%
  mutate(geography = county,
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  ungroup() %>%
  select(year, geography, pres_class, acres, pres_percent)

temp_3_region_total <- pres_co_sum %>% group_by(year) %>%
  summarize(acres = sum(acres),
            pres_percent = acres / sum(counties$area_acres)) %>%
  mutate(geography = "DVRPC",
         pres_class = "Total",
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  select(year, geography, pres_class, acres, pres_percent)

temp_3_subregion_total <- pres_sub_sum %>% group_by(year, subregion) %>%
  summarize(acres = sum(acres), 
            total_acres = first(area_total), 
            pres_percent = acres / total_acres) %>%
  mutate(geography = subregion,
         pres_class = "Total",
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  ungroup() %>%
  select(year, geography, pres_class, acres, pres_percent) %>%
  filter(geography != "Philadelphia")

temp_3_pa_total <- pres_pa_sum %>% group_by(year, planning_area) %>%
  summarize(acres = sum(acres), 
            total_acres = first(area_total), 
            pres_percent = acres / total_acres) %>%
  mutate(geography = planning_area,
         pres_class = "Total",
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  ungroup() %>%
  select(year, geography, pres_class, acres, pres_percent)

temp_3_county_total <- pres_co_sum %>% group_by(year, county) %>%
  summarize(acres = sum(acres), 
            total_acres = first(area_total), 
            pres_percent = acres / total_acres) %>%
  mutate(geography = county,
         pres_class = "Total",
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres),
         pres_percent = round(pres_percent, rounding_factor_percent)) %>%
  ungroup() %>%
  select(year, geography, pres_class, acres, pres_percent)

temp_3 <- bind_rows(temp_3_region_class, temp_3_subregion_class, 
                    temp_3_pa_class, temp_3_county_class,
                    temp_3_region_total, temp_3_subregion_total, 
                    temp_3_pa_total, temp_3_county_total)

rm(temp_3_region_class, temp_3_subregion_class, 
   temp_3_pa_class, temp_3_county_class,
   temp_3_region_total, temp_3_subregion_total, 
   temp_3_pa_total, temp_3_county_total)

graph_3A <- temp_3 %>%
  filter(pres_class == "Total") %>%
  select(-pres_class, -pres_percent) %>%
  mutate(geography = paste(geography, "- Total Acres", sep = "")) %>%
  spread(geography, acres) %>%
  select(year, 'DVRPC- Total Acres', 'NJ Suburbs- Total Acres', 'PA Suburbs- Total Acres',
         'Core City- Total Acres', 'Developed Community- Total Acres', 
         'Growing Suburb- Total Acres', 'Rural Area- Total Acres',
         'Bucks- Total Acres', 'Chester- Total Acres', 
         'Delaware- Total Acres', 'Montgomery- Total Acres', 
         'Philadelphia- Total Acres', 'Burlington- Total Acres', 
         'Camden- Total Acres', 'Gloucester- Total Acres', 'Mercer- Total Acres')

graph_3B <- temp_3 %>%
  filter(pres_class == "Total") %>%
  select(-pres_class, -acres) %>%
  mutate(geography = paste(geography, "- Total Percent", sep = ""))%>%
  spread(geography, pres_percent) %>%
  select(year, 'DVRPC- Total Percent', 'NJ Suburbs- Total Percent', 'PA Suburbs- Total Percent',
         'Core City- Total Percent', 'Developed Community- Total Percent', 
         'Growing Suburb- Total Percent', 'Rural Area- Total Percent',
         'Bucks- Total Percent', 'Chester- Total Percent', 
         'Delaware- Total Percent', 'Montgomery- Total Percent', 
         'Philadelphia- Total Percent', 'Burlington- Total Percent', 
         'Camden- Total Percent', 'Gloucester- Total Percent', 'Mercer- Total Percent')

graph_3C <- temp_3 %>%
  filter(pres_class == "Public") %>%
  select(-pres_class, -pres_percent) %>%
  mutate(geography = paste(geography, "- Public Acres", sep = "")) %>%
  spread(geography, acres) %>%
  select(year, 'DVRPC- Public Acres', 'NJ Suburbs- Public Acres', 'PA Suburbs- Public Acres',
         'Core City- Public Acres', 'Developed Community- Public Acres', 
         'Growing Suburb- Public Acres', 'Rural Area- Public Acres',
         'Bucks- Public Acres', 'Chester- Public Acres', 
         'Delaware- Public Acres', 'Montgomery- Public Acres', 
         'Philadelphia- Public Acres', 'Burlington- Public Acres', 
         'Camden- Public Acres', 'Gloucester- Public Acres', 'Mercer- Public Acres')

graph_3D <- temp_3 %>%
  filter(pres_class == "Public") %>%
  select(-pres_class, -acres) %>%
  mutate(geography = paste(geography, "- Public Percent", sep = ""))%>%
  spread(geography, pres_percent) %>%
  select(year, 'DVRPC- Public Percent', 'NJ Suburbs- Public Percent', 'PA Suburbs- Public Percent',
         'Core City- Public Percent', 'Developed Community- Public Percent', 
         'Growing Suburb- Public Percent', 'Rural Area- Public Percent',
         'Bucks- Public Percent', 'Chester- Public Percent', 
         'Delaware- Public Percent', 'Montgomery- Public Percent', 
         'Philadelphia- Public Percent', 'Burlington- Public Percent', 
         'Camden- Public Percent', 'Gloucester- Public Percent', 'Mercer- Public Percent')

graph_3E <- temp_3 %>%
  filter(pres_class == "Private") %>%
  select(-pres_class, -pres_percent) %>%
  mutate(geography = paste(geography, "- Private Acres", sep = "")) %>%
  spread(geography, acres) %>%
  select(year, 'DVRPC- Private Acres', 'NJ Suburbs- Private Acres', 'PA Suburbs- Private Acres',
         'Core City- Private Acres', 'Developed Community- Private Acres', 
         'Growing Suburb- Private Acres', 'Rural Area- Private Acres',
         'Bucks- Private Acres', 'Chester- Private Acres', 
         'Delaware- Private Acres', 'Montgomery- Private Acres', 
         'Philadelphia- Private Acres', 'Burlington- Private Acres', 
         'Camden- Private Acres', 'Gloucester- Private Acres', 'Mercer- Private Acres')

graph_3F <- temp_3 %>%
  filter(pres_class == "Private") %>%
  select(-pres_class, -acres) %>%
  mutate(geography = paste(geography, "- Private Percent", sep = ""))%>%
  spread(geography, pres_percent) %>%
  select(year, 'DVRPC- Private Percent', 'NJ Suburbs- Private Percent', 'PA Suburbs- Private Percent',
         'Core City- Private Percent', 'Developed Community- Private Percent', 
         'Growing Suburb- Private Percent', 'Rural Area- Private Percent',
         'Bucks- Private Percent', 'Chester- Private Percent', 
         'Delaware- Private Percent', 'Montgomery- Private Percent', 
         'Philadelphia- Private Percent', 'Burlington- Private Percent', 
         'Camden- Private Percent', 'Gloucester- Private Percent', 'Mercer- Private Percent')

graph_3 <- left_join(graph_3A, graph_3B, by = "year") %>%
  left_join(graph_3C, by = "year") %>%
  left_join(graph_3D, by = "year") %>%
  left_join(graph_3E, by = "year") %>%
  left_join(graph_3F, by = "year")

rm(graph_3A, graph_3B, graph_3C, graph_3D, graph_3E, graph_3F)
  

#graph 4 all protection types for a single geography
temp_4_region <- pres_co_sum %>% group_by(year, pres_type) %>%
  summarize(acres = sum(acres)) %>%
  mutate(pres_type = paste("DVRPC", pres_type, sep = "- "),
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres)) %>%
  spread(pres_type, acres)

temp_4_subregion <- pres_sub_sum %>% 
  select(year, subregion, pres_type, acres) %>%
  mutate(pres_type = paste(subregion, pres_type, sep = "- "),
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres)) %>%
  select(-subregion) %>%
  spread(pres_type, acres) %>%
  select(-'Philadelphia- County',
         -'Philadelphia- Federal',
         -'Philadelphia- Municipal',
         -'Philadelphia- Non-profit',
         -'Philadelphia- State')

temp_4_pa <- pres_pa_sum %>% 
  select(year, planning_area, pres_type, acres) %>%
  mutate(pres_type = paste(planning_area, pres_type, sep = "- "),
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres)) %>%
  select(-planning_area) %>%
  spread(pres_type, acres)

temp_4_county <- pres_co_sum %>% 
  select(year, county, pres_type, acres) %>%
  mutate(pres_type = paste(county, pres_type, sep = "- "),
         acres = acres / scaling_factor_acres,
         acres = round(acres, rounding_factor_acres)) %>%
  select(-county) %>%
  spread(pres_type, acres)

graph_4 <- left_join(temp_4_region, temp_4_subregion, by = "year") %>%
  left_join(temp_4_pa, by = "year") %>%
  left_join(temp_4_county, by = "year")

rm(temp_4_region, temp_4_subregion, temp_4_pa, temp_4_county)

graph_4[is.na(graph_4)] <- 0

graph_4 %<>% 
  mutate('Bucks- Federal' = 0,
         'Camden- Federal' = 0, 
         'Gloucester- Federal' = 0,
         'Mercer- Federal' = 0,
         'Philadelphia- Preserved Farmland' = 0,
         'Philadelphia- Municipal' = 0) %>%
  select('year', 
         'DVRPC- Federal','DVRPC- State',
         'DVRPC- County', 'DVRPC- Municipal', 
         'DVRPC- Non-profit', 'DVRPC- Preserved Farmland',
         'NJ Suburbs- Federal', 'NJ Suburbs- State',
         'NJ Suburbs- County', 'NJ Suburbs- Municipal',
         'NJ Suburbs- Non-profit', 'NJ Suburbs- Preserved Farmland',
         'PA Suburbs- Federal','PA Suburbs- State',
         'PA Suburbs- County', 'PA Suburbs- Municipal',
         'PA Suburbs- Non-profit', 'PA Suburbs- Preserved Farmland', 
         'Core City- Federal','Core City- State',
         'Core City- County', 'Core City- Municipal', 
         'Core City- Non-profit', 'Core City- Preserved Farmland',
         'Developed Community- Federal', 'Developed Community- State',
         'Developed Community- County', 'Developed Community- Municipal',
         'Developed Community- Non-profit', 'Developed Community- Preserved Farmland', 
         'Growing Suburb- Federal', 'Growing Suburb- State',
         'Growing Suburb- County', 'Growing Suburb- Municipal', 
         'Growing Suburb- Non-profit', 'Growing Suburb- Preserved Farmland', 
         'Rural Area- Federal', 'Rural Area- State',         
         'Rural Area- County', 'Rural Area- Municipal', 
         'Rural Area- Non-profit', 'Rural Area- Preserved Farmland', 
         'Bucks- Federal', 'Bucks- State', 
         'Bucks- County', 'Bucks- Municipal',
         'Bucks- Non-profit', 'Bucks- Preserved Farmland',
         'Burlington- Federal', 'Burlington- State', 
         'Burlington- County', 'Burlington- Municipal',
         'Burlington- Non-profit', 'Burlington- Preserved Farmland',
         'Camden- Federal', 'Camden- State',         
         'Camden- County', 'Camden- Municipal', 
         'Camden- Non-profit', 'Camden- Preserved Farmland', 
         'Chester- Federal', 'Chester- State', 
         'Chester- County', 'Chester- Municipal', 
         'Chester- Non-profit', 'Chester- Preserved Farmland',
         'Delaware- Federal', 'Delaware- State', 
         'Delaware- County', 'Delaware- Municipal', 
         'Delaware- Non-profit', 'Delaware- Preserved Farmland', 
         'Gloucester- Federal', 'Gloucester- State', 
         'Gloucester- County', 'Gloucester- Municipal', 
         'Gloucester- Non-profit', 'Gloucester- Preserved Farmland', 
         'Mercer- Federal', 'Mercer- State', 
         'Mercer- County', 'Mercer- Municipal', 
         'Mercer- Non-profit', 'Mercer- Preserved Farmland', 
         'Montgomery- Federal', 'Montgomery- State', 
         'Montgomery- County', 'Montgomery- Municipal',
         'Montgomery- Non-profit', 'Montgomery- Preserved Farmland', 
         'Philadelphia- Federal', 'Philadelphia- State',
         'Philadelphia- County', 'Philadelphia- Municipal',
         'Philadelphia- Non-profit', 'Philadelphia- Preserved Farmland')

#export CSVs
write_csv(graph_3, "Outputs/Land Consumption/land consumption graph_3.csv")
write_csv(graph_4, "Outputs/Land Consumption/land consumption graph_4.csv")