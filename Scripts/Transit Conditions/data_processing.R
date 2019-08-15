#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl", "magrittr")
pack(packages)
rm(pack, packages)

source("data_import.R")

#replace vehicle-type codes with readable values
vehicleAge$vehicle %<>% str_replace_all(c(
  "AB" = "Articulated Buses",
  "AO" = "Automobiles",
  "BA" = "Buses",
  "BB" = "Buses",
  "BC" = "Buses",
  "BU" = "Buses",
  "LR" = "Light Rail Vehicles",
  "RL" = "Commuter Rail Locomotives",
  "RP" = "Commuter Rail Passenger Coaches",
  "RS" = "Commuter Rail Self-Propelled Passenger Coaches",
  "VN" = "Vans",
  "HR" = "Heavy Rail Passenger Cars",
  "TB" = "TrolleyBuses",
  #"OR" = "",
  "VT" = "Vintage Trolleys"))

#replace vehicle types with consistent capitalization and pluralization
vehicleAge$vehicle %<>% str_to_title()
vehicleAge$vehicle %<>% str_replace_all(c(
  "Articulated Bus$" = "Articulated Buses",
  "Commuter Rail Passenger Coach$" = "Commuter Rail Passenger Coaches",
  "Commuter Rail Self-Propelled Passenger Car$" = "Commuter Rail Self-Propelled Passenger Cars",
  "Commuter Rail Locomotive$" = "Commuter Rail Locomotives",
  "Light Rail Vehicle$" = "Light Rail Vehicles",
  "Vintage Trolley$" = "Vintage Trolleys",
  "Heavy Rail Passenger Car$" = "Heavy Rail Passenger Cars",
  "Van$" = "Vans",
  "Bus$" = "Buses",
  "Trolleybus$" = "Trolleybuses",
  "Automobile$" = "Automobiles"))

#replace vehicle types with consistent nomenclature
vehicleAge$vehicle %<>% str_replace_all(c(
  "Commuter Rail Self-Propelled Passenger Cars$" = "Commuter Rail Self-Propelled Passenger Coaches",
  "Light Rail Vehicles \\(Streetcars\\)" = "Light Rail Vehicles",
  "Cutaway$" = "Vans",
  "Heavy Rail Passenger Cars" = "Heavy Rail Passenger Coaches"))

#add broad vehicle classes
buses <- c("Articulated Buses", "Buses", "Trolleybuses", "Over-The-Road Buses")
rails <- c("Light Rail Vehicles", "Commuter Rail Locomotives", "Commuter Rail Passenger Coaches",
           "Commuter Rail Self-Propelled Passenger Coaches", "Heavy Rail Passenger Coaches", "Or",
           "Vintage Trolleys")
other <- c("Automobiles", "Vans")

for(i in 1:length(vehicleAge$vehicle)){
  if(vehicleAge$vehicle[i] %in% buses){vehicleAge$class[i] <- "Buses"}
  if(vehicleAge$vehicle[i] %in% rails){vehicleAge$class[i] <- "Rail Vehicles"}
  if(vehicleAge$vehicle[i] %in% other){vehicleAge$class[i] <- "Other"}
}

rm(i, buses, rails, other)

#rearrange columns
vehicleAge %<>% select(year, agencyCode, agency_name, class, vehicle, nVehicles, everything())



#summary table
summary_table <- vehicleAge %>% 
  group_by(class, year) %>% 
  summarize(nVehicles_sum = sum(nVehicles), avgAge = weighted.mean(avgAge, nVehicles),
            age_0_5 = sum(age_0_5), age_6_11 = sum(age_6_11), age_12_15 = sum(age_12_15),
            age_16_20 = sum(age_16_20), age_21_25 = sum(age_21_25), age_25over = sum(age_25over)) %>%
  filter(class %in% c("Buses", "Rail Vehicles"))

sum_table_gathered <- summary_table %>%
  gather("age_cat", "n_in_cat", 5:10) %>%
  mutate(age_cat = factor(age_cat, 
                          levels = c("age_0_5", "age_6_11", "age_12_15", 
                                     "age_16_20", "age_21_25", "age_25over"))) 

