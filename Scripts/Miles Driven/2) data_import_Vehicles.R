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

#import raw 2005-2017 ACS data, add year variable
vehiclesACS <- data.frame(NULL)

for(i in 1:13){
  year <- c("05", "06", "07", "08", "09", "10", 
            "11", "12", "13", "14", "15", "16", "17")
  
  filename <- paste("../Data/Vehicle Data/ACS_", year[i], "_1YR_B25046_with_ann.csv", sep = "")
  
  temp <- read_csv(filename)[-1,]
  
  temp$year <- as.numeric(paste("20", year[i], sep = ""))
  
  vehiclesACS %<>% bind_rows(temp)
}
rm(year, filename, i, temp)

#rename ACS variables
vehiclesACS %<>% rename(county = `GEO.display-label`, vehiclesTotal = HD01_VD01, 
                        vehiclesTotMOE = HD02_VD01, vehiclesOwnerOcc = HD01_VD02, 
                        vehiclesOwnerOccMOE = HD02_VD02, vehiclesRenterOcc = HD01_VD03, 
                        vehiclesRenterOccMOE = HD02_VD03)

#import raw 2000 Census data
vehicles2000 <- read_csv("../Data/Vehicle Data/DEC_00_SF3_H046_with_ann.csv")[-1,]
vehicles2000$year <- 2000

#rename Census variables
vehicles2000 %<>% rename(county = `GEO.display-label`, vehiclesTotal = VD01, 
                         vehiclesOwnerOcc = VD02, vehiclesRenterOcc = VD03)

#combine census and ACS data
vehicles <- bind_rows(vehicles2000, vehiclesACS) %>% 
            select(county, year, vehiclesTotal)

rm(vehicles2000, vehiclesACS)

#trim county names, convert vehicle total to numeric
vehicles$county %<>% str_replace(" County, New Jersey", "")

vehicles$county %<>% str_replace(" County, Pennsylvania", "")

vehicles$vehiclesTotal %<>% as.numeric()

