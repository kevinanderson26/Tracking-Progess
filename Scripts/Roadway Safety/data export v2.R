#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl")
pack(packages)
rm(pack, packages)

#source processed data
source("data_processing.R")

###Graph 1: 5-year KSI trends, toggle between ksi/VMT, ksi/Capita, and total KSI

# create Graph 1 dataframes for each geography type starting in 2010 
# and adding consistently named geography variable
graph_1_region <- ksiRegion %>%
  select(year, ksi5yrAvg, ksiPerPop5yrAvg, ksiPerVMT5yrAvg) %>%
  mutate(geography = "DVRPC") %>%
  select(year, geography, everything()) %>%
  filter(year >= 2010)

graph_1_subregion <- ksiSubregion %>%
  select(year, subregion, ksi5yrAvg, ksiPerPop5yrAvg, ksiPerVMT5yrAvg) %>%
  rename(geography = subregion) %>%
  filter(year >= 2010 & geography != "Philadelphia")

graph_1_county <- ksiCounty %>%
  select(year, county, ksi5yrAvg, ksiPerPop5yrAvg, ksiPerVMT5yrAvg) %>%
  rename(geography = county) %>%
  filter(year >= 2010)

#bind all graph 1 dataframes together and remove individual data frames
graph_1 <- bind_rows(graph_1_region, graph_1_subregion, graph_1_county)
rm(graph_1_region, graph_1_subregion, graph_1_county)

#cut graph 1 combined data frames into variable types, combine geography and variable type names, spread
graph_1_tot_ksi <- graph_1 %>%
  select(year, geography, ksi5yrAvg) %>%
  mutate(geography = paste(geography, "- ksi total", sep = "")) %>%
  spread(geography, ksi5yrAvg)

graph_1_ksi_cap <- graph_1 %>%
  select(year, geography, ksiPerPop5yrAvg) %>%
  mutate(geography = paste(geography, "- ksi per capita", sep = "")) %>%
  spread(geography, ksiPerPop5yrAvg)

graph_1_ksi_VMT <- graph_1 %>%
  select(year, geography, ksiPerVMT5yrAvg) %>%
  mutate(geography = paste(geography, "- ksi per VMT", sep = "")) %>%
  spread(geography, ksiPerVMT5yrAvg)

#join all spread tables
graph_1_spread <- left_join(graph_1_tot_ksi, graph_1_ksi_cap, by = "year") %>%
  left_join(graph_1_ksi_VMT)

#remove individual spread tables
rm(graph_1_tot_ksi, graph_1_ksi_VMT, graph_1_ksi_cap)

###Graph 2: stacked bar of vulnerable user / motor vehicle KSI per geography

# create Graph 2 dataframes for each geography type starting in 2010 
# and adding consistently named geography variable
graph_2_region <- ksiRegion %>%
  select(year, ksiBikePed, ksiMotorVehicle, ksiBikePedPerPop, ksiMotorVehiclePerPop, ksiBikePedPerVMT, ksiMotorVehiclePerVMT) %>%
  mutate(geography = "DVRPC") %>%
  select(year, geography, everything())
  
graph_2_subregion <- ksiSubregion %>%
  select(year, subregion, ksiBikePed, ksiMotorVehicle, ksiBikePedPerPop, ksiMotorVehiclePerPop, ksiBikePedPerVMT, ksiMotorVehiclePerVMT) %>%
  rename(geography = subregion) %>%
  filter(geography != "Philadelphia")

graph_2_county <- ksiCounty %>%
  select(year, county, ksiBikePed, ksiMotorVehicle, ksiBikePedPerPop, ksiMotorVehiclePerPop, ksiBikePedPerVMT, ksiMotorVehiclePerVMT) %>%
  rename(geography = county)

#bind all graph 1 dataframes together and remove individual data frames
graph_2 <- bind_rows(graph_2_region, graph_2_subregion, graph_2_county)
rm(graph_2_region, graph_2_subregion, graph_2_county)

#cut graph 2 combined data frames into variable types, combine geography and variable type names, spread
graph_2_ksiBikePed <- graph_2 %>% 
  select(year, geography, ksiBikePed) %>%
  mutate(geography = paste(geography, "- ksiBikePed", sep = "")) %>%
  spread(geography, ksiBikePed)
  
graph_2_ksiMotorVehicle <- graph_2 %>% 
  select(year, geography, ksiMotorVehicle) %>%
  mutate(geography = paste(geography, "- ksiMotorVehicle", sep = "")) %>%
  spread(geography, ksiMotorVehicle)

graph_2_ksiBikePedPerPop <- graph_2 %>% 
  select(year, geography, ksiBikePedPerPop) %>%
  mutate(geography = paste(geography, "- ksiBikePed", sep = "")) %>%
  spread(geography, ksiBikePedPerPop)

graph_2_ksiMotorVehiclePerPop <- graph_2 %>% 
  select(year, geography, ksiMotorVehiclePerPop) %>%
  mutate(geography = paste(geography, "- ksiMotorVehicle", sep = "")) %>%
  spread(geography, ksiMotorVehiclePerPop)

graph_2_ksiBikePedPerVMT <- graph_2 %>% 
  select(year, geography, ksiBikePedPerVMT) %>%
  mutate(geography = paste(geography, "- ksiBikePed", sep = "")) %>%
  spread(geography, ksiBikePedPerVMT)

graph_2_ksiMotorVehiclePerVMT <- graph_2 %>% 
  select(year, geography, ksiMotorVehiclePerVMT) %>%
  mutate(geography = paste(geography, "- ksiMotorVehicle", sep = "")) %>%
  spread(geography, ksiMotorVehiclePerVMT)

# join all spread tables into different dataframes based on web toggle
# graph 2A: total ksi
# graph 2B: ksi per capita
# graph 2C: ksi per VMT
graph_2A <- left_join(graph_2_ksiBikePed, graph_2_ksiMotorVehicle, by = "year") %>%
  select(year,
         'DVRPC- ksiBikePed', 'DVRPC- ksiMotorVehicle',
         'PA Suburbs- ksiBikePed', 'PA Suburbs- ksiMotorVehicle',
         'NJ Suburbs- ksiBikePed', 'NJ Suburbs- ksiMotorVehicle',
         'Bucks- ksiBikePed', 'Bucks- ksiMotorVehicle',
         'Burlington- ksiBikePed', 'Burlington- ksiMotorVehicle',
         'Camden- ksiBikePed', 'Camden- ksiMotorVehicle',
         'Chester- ksiBikePed', 'Chester- ksiMotorVehicle',
         'Delaware- ksiBikePed', 'Delaware- ksiMotorVehicle',
         'Gloucester- ksiBikePed', 'Gloucester- ksiMotorVehicle',
         'Mercer- ksiBikePed', 'Mercer- ksiMotorVehicle',
         'Montgomery- ksiBikePed', 'Montgomery- ksiMotorVehicle',
         'Philadelphia- ksiBikePed', 'Philadelphia- ksiMotorVehicle')

graph_2B <- left_join(graph_2_ksiBikePedPerPop, graph_2_ksiMotorVehiclePerPop, by = "year") %>%
  select(year,
         'DVRPC- ksiBikePed', 'DVRPC- ksiMotorVehicle',
         'PA Suburbs- ksiBikePed', 'PA Suburbs- ksiMotorVehicle',
         'NJ Suburbs- ksiBikePed', 'NJ Suburbs- ksiMotorVehicle',
         'Bucks- ksiBikePed', 'Bucks- ksiMotorVehicle',
         'Burlington- ksiBikePed', 'Burlington- ksiMotorVehicle',
         'Camden- ksiBikePed', 'Camden- ksiMotorVehicle',
         'Chester- ksiBikePed', 'Chester- ksiMotorVehicle',
         'Delaware- ksiBikePed', 'Delaware- ksiMotorVehicle',
         'Gloucester- ksiBikePed', 'Gloucester- ksiMotorVehicle',
         'Mercer- ksiBikePed', 'Mercer- ksiMotorVehicle',
         'Montgomery- ksiBikePed', 'Montgomery- ksiMotorVehicle',
         'Philadelphia- ksiBikePed', 'Philadelphia- ksiMotorVehicle')
         
graph_2C <- left_join(graph_2_ksiBikePedPerVMT, graph_2_ksiMotorVehiclePerVMT, by = "year") %>%
  select(year,
         'DVRPC- ksiBikePed', 'DVRPC- ksiMotorVehicle',
         'PA Suburbs- ksiBikePed', 'PA Suburbs- ksiMotorVehicle',
         'NJ Suburbs- ksiBikePed', 'NJ Suburbs- ksiMotorVehicle',
         'Bucks- ksiBikePed', 'Bucks- ksiMotorVehicle',
         'Burlington- ksiBikePed', 'Burlington- ksiMotorVehicle',
         'Camden- ksiBikePed', 'Camden- ksiMotorVehicle',
         'Chester- ksiBikePed', 'Chester- ksiMotorVehicle',
         'Delaware- ksiBikePed', 'Delaware- ksiMotorVehicle',
         'Gloucester- ksiBikePed', 'Gloucester- ksiMotorVehicle',
         'Mercer- ksiBikePed', 'Mercer- ksiMotorVehicle',
         'Montgomery- ksiBikePed', 'Montgomery- ksiMotorVehicle',
         'Philadelphia- ksiBikePed', 'Philadelphia- ksiMotorVehicle')

#remove individual spread tables
rm(graph_2_ksiBikePed, graph_2_ksiMotorVehicle, graph_2_ksiBikePedPerPop, 
   graph_2_ksiMotorVehiclePerPop, graph_2_ksiBikePedPerVMT, graph_2_ksiMotorVehiclePerVMT)

#write CSVs for web
write_csv(graph_1_spread, "../Processed Data/crashes Web 1.csv")
write_csv(graph_2A, "../Processed Data/crashes Web 2A.csv")
write_csv(graph_2B, "../Processed Data/crashes Web 2B.csv")
write_csv(graph_2C, "../Processed Data/crashes Web 2C.csv")