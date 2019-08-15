#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr")
pack(packages)
rm(pack, packages)

#source input files
source("1) data_import_VMT.R")
source("2) data_import_Vehicles.R")
source("3) data_import_Population.R")

#join vmt, vehicles, and population data frames, filter to 2000 and later
all_vmt <- full_join(pop, vmt, by = c("county", "year")) %>%
              full_join(vehicles, by = c("county", "year")) %>%
              filter(year >= 2000)

#remove individual variable data frames
rm(pop, vehicles, vmt)

#fill missing geo variables
all_vmt$state <- ifelse(all_vmt$county %in% c("Bucks", "Chester", "Delaware", "Montgomery", "Philadelphia"), "Pennsylvania", "New Jersey") 

all_vmt$stateCode <- ifelse(all_vmt$state == "Pennsylvania", 42, 34)

all_vmt$countyCode <- ifelse(all_vmt$county == "Bucks", 17, 
                  ifelse(all_vmt$county == "Chester", 29, 
                  ifelse(all_vmt$county == "Delaware", 45, 
                  ifelse(all_vmt$county == "Montgomery", 91, 
                  ifelse(all_vmt$county == "Philadelphia", 101, 
                  ifelse(all_vmt$county == "Burlington", 5,
                  ifelse(all_vmt$county == "Camden", 7,
                  ifelse(all_vmt$county == "Gloucester", 15, 21))))))))

all_vmt$geoid <- ifelse(all_vmt$county == "Bucks", 42017, 
                  ifelse(all_vmt$county == "Chester", 42029, 
                  ifelse(all_vmt$county == "Delaware", 42045, 
                  ifelse(all_vmt$county == "Montgomery", 42091, 
                  ifelse(all_vmt$county == "Philadelphia", 42101, 
                  ifelse(all_vmt$county == "Burlington", 34005,
                  ifelse(all_vmt$county == "Camden", 34007,
                  ifelse(all_vmt$county == "Gloucester", 34015, 34021))))))))

