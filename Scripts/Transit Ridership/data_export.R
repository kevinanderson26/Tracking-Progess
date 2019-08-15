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

source("Data_processing.R")

#graph 1: total trips and total trips per capita

scaling_factor_total <- 1000
scaling_factor_PerCap <- 1
rounding_factor_total <- 1
rounding_factor_PerCap <- 2

transit_ridership_graph_1 <- total_trips %>% 
  filter(year >= 2000) %>%
  select(year, unlinkedTrips, unlinkedTripsPerCap) %>%
  mutate(unlinkedTrips = unlinkedTrips / scaling_factor_total,
         unlinkedTrips = round(unlinkedTrips, rounding_factor_total),
         unlinkedTripsPerCap = unlinkedTripsPerCap / scaling_factor_PerCap,
         unlinkedTripsPerCap = round(unlinkedTripsPerCap, rounding_factor_PerCap))

#graph 2: trips per mode
transit_ridership_graph_2a <- unlinkedMode %>% select(year, mode_name, unlinkedTrips) %>%
  filter(year >= 2000) %>%
  mutate(unlinkedTrips = unlinkedTrips / scaling_factor_total,
         unlinkedTrips = round(unlinkedTrips, rounding_factor_total)) %>%
  spread(mode_name, unlinkedTrips) %>%
  replace_na(list(Trolleybus = 0))

transit_ridership_graph_2b <- unlinkedMode %>% select(year, mode_name, unlinkedTripsPerCap) %>%
  filter(year >= 2000) %>%
  mutate(unlinkedTripsPerCap = unlinkedTripsPerCap / scaling_factor_PerCap,
         unlinkedTripsPerCap = round(unlinkedTripsPerCap, rounding_factor_PerCap),
         mode_name = paste(mode_name, "-per capita", sep = "")) %>%
  spread(mode_name, unlinkedTripsPerCap) %>%
  replace_na(list('Trolleybus-per capita' = 0))

transit_ridership_graph_2 <- left_join(transit_ridership_graph_2a, transit_ridership_graph_2b, by = "year")

rm(transit_ridership_graph_2a, transit_ridership_graph_2b)

#graph 3: trips per agency
transit_ridership_graph_3a <- unlinkedAgency %>% select(year, agency_name, unlinkedTrips) %>%
  filter(year >= 2000) %>%
  mutate(unlinkedTrips = unlinkedTrips / scaling_factor_total,
         unlinkedTrips = round(unlinkedTrips, rounding_factor_total)) %>%
  spread(agency_name, unlinkedTrips)

transit_ridership_graph_3b <- unlinkedAgency %>% select(year, agency_name, unlinkedTripsPerCap) %>%
  filter(year >= 2000) %>%
  mutate(unlinkedTripsPerCap = unlinkedTripsPerCap / scaling_factor_PerCap,
         unlinkedTripsPerCap = round(unlinkedTripsPerCap, rounding_factor_PerCap),
         agency_name = paste(agency_name, "-per capita", sep = "")) %>%
  spread(agency_name, unlinkedTripsPerCap)

transit_ridership_graph_3 <- left_join(transit_ridership_graph_3a, transit_ridership_graph_3b, by = "year")

rm(transit_ridership_graph_3a, transit_ridership_graph_3b)

#export CSVs
write_csv(transit_ridership_graph_1, "../Processed Data/transit_ridership_graph_1.csv")
write_csv(transit_ridership_graph_2, "../Processed Data/transit_ridership_graph_2.csv")
write_csv(transit_ridership_graph_3, "../Processed Data/transit_ridership_graph_3.csv")



