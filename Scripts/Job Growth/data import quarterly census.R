#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl", "lubridate")
pack(packages)
rm(pack, packages)

raw <- data.frame()
counties <- c("bucks", "chester", "delaware", "montgomery","philadelphia", 
              "burlington", "camden", "gloucester", "mercer")

for(i in 1:length(counties)){
  file_name <- paste("../Data/Total Jobs/monthly_total_", counties[i], ".xlsx", sep = "")
  temp <- read_excel(file_name, skip = 13) %>%
    select(year = Year, jobs = Annual) %>%
    filter(year < 2018)
  temp$county <- counties[i] %>% str_to_title()
  raw %<>% bind_rows(temp)}


qcew_county <- raw %>% select(year, county, jobs)
rm(i, temp, file_name, counties, raw)

#add subregional field
nj_counties <- c("Burlington", "Camden", "Gloucester", "Mercer")
qcew_county$subregion <- ifelse(qcew_county$county == "Philadelphia", "Philadelphia Subregion",
                         ifelse(qcew_county$county %in% nj_counties, "New Jersey Suburbs", 
                                "Pennsylvania Suburbs"))
rm(nj_counties)
