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

#initialize empty data frames
renter_county <- data.frame()
renter_munic <- data.frame()
mortgage_county <- data.frame()
no_mortgage_county <- data.frame()
mortgage_munic <- data.frame()
no_mortgage_munic <- data.frame()

#define cost bracket names
renter_names <- c("HD01_VD02" = "0.0-9.9%", "HD01_VD03" = "10.0-14.9%", 
                  "HD01_VD04" = "15.0-19.9%", "HD01_VD05" = "20.0-24.9%", 
                  "HD01_VD06" = "25.0-29.9%", "HD01_VD07" = "30.0-34.9%", 
                  "HD01_VD08" = "35.0-39.9%", "HD01_VD09" = "40.0-49.9%", 
                  "HD01_VD10" = "50%+", "HD01_VD11" = "Not Computed")

mortgage_names <- c("HD01_VD03" = "0.0-9.9%", "HD01_VD04" = "10.0-14.9%", 
                    "HD01_VD05" = "15.0-19.9%", "HD01_VD06" = "20.0-24.9%", 
                    "HD01_VD07" = "25.0-29.9%", "HD01_VD08" = "30.0-34.9%", 
                    "HD01_VD09" = "35.0-39.9%", "HD01_VD10" = "40.0-49.9%", 
                    "HD01_VD11" = "50%+", "HD01_VD12" = "Not Computed")

no_mortgage_names <- c("HD01_VD14" = "0.0-9.9%", "HD01_VD15" = "10.0-14.9%", 
                       "HD01_VD16" = "15.0-19.9%", "HD01_VD17" = "20.0-24.9%", 
                       "HD01_VD18" = "25.0-29.9%", "HD01_VD19" = "30.0-34.9%", 
                       "HD01_VD20" = "35.0-39.9%", "HD01_VD21" = "40.0-49.9%", 
                       "HD01_VD22" = "50%+", "HD01_VD23" = "Not Computed")

#import raw county renter data
for(i in 2005:2017){
  two_digit <- i %>% as.character() %>% str_sub(3, 4)
  if(i <= 2006){
    filename <- paste("../Data/Renters/County/ACS_", two_digit, "_EST_B25070_with_ann.csv", sep = "")}
  if(i > 2006){
    filename <- paste("../Data/Renters/County/ACS_", two_digit, "_1YR_B25070_with_ann.csv", sep = "")}
  temp <- read_csv(filename) %>% 
    filter(GEO.id != "Id") %>%
    select(geoid = GEO.id2, co_name = 'GEO.display-label', HD01_VD02, HD01_VD03, HD01_VD04, 
           HD01_VD05, HD01_VD06, HD01_VD07, HD01_VD08, HD01_VD09, HD01_VD10, HD01_VD11) %>%
    gather("cost_bracket", "n", 3: 12) %>%
    mutate(year = i, type = "Renter") %>%
    select(co_name, geoid, year, type, cost_bracket, n)
  temp$cost_bracket %<>% str_replace_all(renter_names)
  renter_county %<>% bind_rows(temp)}

#import raw municipal renter data
for(i in 2009:2017){
  two_digit <- i %>% as.character() %>% str_sub(3, 4)
  filename <- paste("../Data/Renters/Municipality/ACS_", two_digit, "_5YR_B25070_with_ann.csv", sep = "")
  temp <- read_csv(filename) %>% 
    filter(GEO.id != "Id") %>%
    select(geoid = GEO.id2, munic_name = 'GEO.display-label', HD01_VD02, HD01_VD03, HD01_VD04, 
           HD01_VD05, HD01_VD06, HD01_VD07, HD01_VD08, HD01_VD09, HD01_VD10, HD01_VD11) %>%
    gather("cost_bracket", "n", 3: 12) %>%
    mutate(year = i, type = "Renter") %>%
    select(munic_name, geoid, year, type, cost_bracket, n)
  temp$cost_bracket %<>% str_replace_all(renter_names)
  renter_munic %<>% bind_rows(temp)}

#import raw county owner w/ mortgage data
for(i in 2005:2017){
  two_digit <- i %>% as.character() %>% str_sub(3, 4)
  if(i <= 2006){
    filename <- paste("../Data/Homeowners/County/ACS_", two_digit, "_EST_B25091_with_ann.csv", sep = "")}
  if(i > 2006){
    filename <- paste("../Data/Homeowners/County/ACS_", two_digit, "_1YR_B25091_with_ann.csv", sep = "")}
  temp <- read_csv(filename) %>% 
    filter(GEO.id != "Id") %>%
    select(geoid = GEO.id2, co_name = 'GEO.display-label', HD01_VD03, HD01_VD04, HD01_VD05, 
           HD01_VD06, HD01_VD07, HD01_VD08, HD01_VD09, HD01_VD10, HD01_VD11, HD01_VD12) %>%
    gather("cost_bracket", "n", 3: 12) %>%
    mutate(year = i, type = "Mortgage") %>%
    select(co_name, geoid, year, type, cost_bracket, n)
  temp$cost_bracket %<>% str_replace_all(mortgage_names)
  mortgage_county %<>% bind_rows(temp)}

#import raw county owner w/o mortgage data
for(i in 2005:2017){
  two_digit <- i %>% as.character() %>% str_sub(3, 4)
  if(i <= 2006){
    filename <- paste("../Data/Homeowners/County/ACS_", two_digit, "_EST_B25091_with_ann.csv", sep = "")}
  if(i > 2006){
    filename <- paste("../Data/Homeowners/County/ACS_", two_digit, "_1YR_B25091_with_ann.csv", sep = "")}
  temp <- read_csv(filename) %>% 
    filter(GEO.id != "Id") %>%
    select(geoid = GEO.id2, co_name = 'GEO.display-label', HD01_VD14, HD01_VD15, HD01_VD16,
           HD01_VD17, HD01_VD18, HD01_VD19, HD01_VD20, HD01_VD21, HD01_VD22,HD01_VD23)%>%
    gather("cost_bracket", "n", 3: 12) %>%
    mutate(year = i, type = "No Mortgage") %>% 
    select(co_name, geoid, year, type, cost_bracket, n)
  temp$cost_bracket %<>% str_replace_all(no_mortgage_names)
  no_mortgage_county %<>% bind_rows(temp)}

#import raw municipal owner w/ mortgage data
for(i in 2009:2017){
  two_digit <- i %>% as.character() %>% str_sub(3, 4)
  filename <- paste("../Data/Homeowners/Municipality/ACS_", two_digit, "_5YR_B25091_with_ann.csv", sep = "")
  temp <- read_csv(filename) %>% 
    filter(GEO.id != "Id") %>%
    select(geoid = GEO.id2, munic_name = 'GEO.display-label', HD01_VD03, HD01_VD04, HD01_VD05, 
           HD01_VD06, HD01_VD07, HD01_VD08, HD01_VD09, HD01_VD10, HD01_VD11, HD01_VD12) %>%
    gather("cost_bracket", "n", 3: 12) %>%
    mutate(year = i, type = "Mortgage") %>%
    select(munic_name, geoid, year, type, cost_bracket, n)
  
  temp$cost_bracket %<>% str_replace_all(mortgage_names)
  mortgage_munic %<>% bind_rows(temp)}

#import raw municipal owner w/o mortgage data
for(i in 2009:2017){
  two_digit <- i %>% as.character() %>% str_sub(3, 4)
  filename <- paste("../Data/Homeowners/Municipality/ACS_", two_digit, "_5YR_B25091_with_ann.csv", sep = "")
  temp <- read_csv(filename) %>% 
    filter(GEO.id != "Id") %>%
    select(geoid = GEO.id2, munic_name = 'GEO.display-label', HD01_VD14, HD01_VD15, HD01_VD16,
           HD01_VD17, HD01_VD18, HD01_VD19, HD01_VD20, HD01_VD21, HD01_VD22,HD01_VD23)%>%
    gather("cost_bracket", "n", 3: 12) %>%
    mutate(year = i, type = "No Mortgage") %>%
    select(munic_name, geoid, year, type, cost_bracket, n)
  
  temp$cost_bracket %<>% str_replace_all(no_mortgage_names)
  no_mortgage_munic %<>% bind_rows(temp)}

#bind rows by geography
afford_county <- bind_rows(renter_county, mortgage_county, no_mortgage_county) 
afford_munic <- bind_rows(renter_munic, mortgage_munic, no_mortgage_munic) 

#convert data types
afford_county$n %<>% as.numeric()
afford_munic$n %<>% as.numeric()

#shorten county names
afford_county$co_name %<>% str_replace_all(c(", Pennsylvania" = "", ", New Jersey" = ""))

#add subregion field to county table
afford_county$subregion <- ifelse(str_detect(afford_county$geoid, "^34"), "New Jersey Suburbs",
                          ifelse(str_detect(afford_county$geoid, "^42101"), "Philadelphia Subregion", 
                                 "Pennsylvania Suburbs"))

#add planning area field to munic table
planning_areas <- read_excel("../Data/2045 Planning Areas.xlsx") %>% 
  select(geoid = GEOID, planning_area = PA_2045)
planning_areas$geoid %<>% as.character()

afford_munic %<>% left_join(planning_areas, by = "geoid")

#factor income categories
afford_county$cost_bracket <- 
  factor(afford_county$cost_bracket, 
         levels = c("Not Computed", "0.0-9.9%", "10.0-14.9%", "15.0-19.9%",
                    "20.0-24.9%", "25.0-29.9%", "30.0-34.9%", "35.0-39.9%", 
                    "40.0-49.9%", "50%+"))

afford_munic$cost_bracket <- 
  factor(afford_munic$cost_bracket, 
         levels = c("Not Computed", "0.0-9.9%", "10.0-14.9%", "15.0-19.9%",
                    "20.0-24.9%", "25.0-29.9%", "30.0-34.9%", "35.0-39.9%", 
                    "40.0-49.9%", "50%+"))

#add rent/own category to collapse mortgage status into single ownership class
afford_county$ownership <- ifelse(afford_county$type == "Renter", "Rent", "Own")
afford_munic$ownership <- ifelse(afford_munic$type == "Renter", "Rent", "Own")

#add expensive category
expensive <- c("35.0-39.9%", "40.0-40.9%","50%+")

afford_county$expensive <- 
  ifelse(afford_county$cost_bracket %in% expensive, "Above 35%",
         ifelse(afford_county$cost_bracket == "Not Computed", "Not Computed", "Below 35%"))

afford_munic$expensive <- 
  ifelse(afford_munic$cost_bracket %in% expensive, "Above 35%", 
         ifelse(afford_munic$cost_bracket == "Not Computed", "Not Computed", "Below 35%"))

#add combined ownership and expensive category
afford_county$class <- paste(afford_county$expensive, afford_county$ownership, sep = "- ")
afford_munic$class <- paste(afford_munic$expensive, afford_munic$ownership, sep = "- ")

#remove unneeded variables and intermediate data frames
rm(i, two_digit, filename, temp, renter_names, mortgage_names, no_mortgage_names, 
   renter_county, mortgage_county, no_mortgage_county, renter_munic, mortgage_munic, 
   no_mortgage_munic, planning_areas, expensive)

