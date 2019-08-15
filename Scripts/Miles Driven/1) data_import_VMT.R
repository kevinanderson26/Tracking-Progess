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

#import VMT data, gather and rename variables, add annual VMT variable
vmt <- read_excel("../Data/VMT.xlsx") %>% 
  gather(County, milesDaily, -Year) %>%
  rename(year = Year, 
         county = County) %>%
  mutate(milesAnnual = milesDaily * 365)