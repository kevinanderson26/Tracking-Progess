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

source("Data_Processing.R")

total_trips %>%
  ggplot(aes(x = year, y = unlinkedTrips)) +
  geom_line() +
  scale_y_continuous(limits = c(0, max(total_trips$unlinkedTrips)))

total_trips %>%
  ggplot(aes(x = year, y = unlinkedTripsPerCap)) +
  geom_line() +
  scale_y_continuous(limits = c(0, max(total_trips$unlinkedTripsPerCap)))

unlinkedMode %>% 
  ggplot(aes(x = year, y = unlinkedTrips, fill = mode_name)) +
  geom_area()

unlinkedMode %>%
  ggplot(aes(x = year, y = unlinkedTripsPerCap, fill = mode_name)) +
  geom_area()

unlinkedAgency %>%
  ggplot(aes(x = year, y = unlinkedTrips, fill = agency_name)) +
  geom_area()

unlinkedAgency %>%
  ggplot(aes(x = year, y = unlinkedTripsPerCap, fill = agency_name)) +
  geom_area()