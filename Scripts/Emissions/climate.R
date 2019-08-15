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

temperature <- read_xlsx("../Data/Climate Data.xlsx", sheet = 1)

precip <- read_xlsx("../Data/Climate Data.xlsx", sheet = 2)

temp_PA_loess <- loess(PA ~ year, temperature)
predict(temp_PA_loess)