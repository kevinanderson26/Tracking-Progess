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

#import data
cbp_county <- data.frame()

for(i in 2005:2016){
  filename <- paste("../Data/CBP/BP_", i, "_00A1_with_ann.csv", sep = "")
  names <- read_csv(filename, n_max = 0) %>% names()
  temp <- read_csv(filename, skip = 2, col_names = names) %>%
    select(geoid = GEO.id2, county_name = 'GEO.display-label', NAICS_code = NAICS.id, NAICS_name = 'NAICS.display-label', year = YEAR.id, establishments = ESTAB, employees = EMP) %>%
    filter(NAICS_code != "00")
  cbp_county %<>% bind_rows(temp)
  rm(filename, names, temp, i)
}

#shorten county names
cbp_county$county_name %<>% str_replace_all(c(" County, " = "", "New Jersey" = "", "Pennsylvania" = ""))

#replace supressed data codes
cbp_county$employees %<>% str_replace_all(c("j" = "17500", "i" = "7500", "h" = "3750", "g" = "1750", "f" = "750", 
                                     "e" = "375", "c" = "175", "b" = "60", "a" = "10"))
cbp_county$employees %<>% as.numeric()

#add condensed categories
cbp_county$sector <- NA

for(i in 1:length(cbp_county$NAICS_code)){
  if(cbp_county$NAICS_code[i] == "11" )
    {cbp_county$sector[i] <- "Agriculture, forestry, fishing"}
  if(cbp_county$NAICS_code[i] %in% c("21", "22"))
    {cbp_county$sector[i] <- "Mining and utilities"}
  if(cbp_county$NAICS_code[i] %in% c("23", "31-33"))
    {cbp_county$sector[i] <- "Construction and manufacturing"}
  if(cbp_county$NAICS_code[i] %in% c("42", "44-45"))
    {cbp_county$sector[i] <- "Wholesale and retail trade"}
  if(cbp_county$NAICS_code[i] == "48-49")
    {cbp_county$sector[i] <- "Transportation and warehousing"}
  if(cbp_county$NAICS_code[i] == "51")
    {cbp_county$sector[i] <- "Information Technology"}
  if(cbp_county$NAICS_code[i] %in% c("52", "53") )
    {cbp_county$sector[i] <- "Finance, Insurance, and Real Estate"}
  if(cbp_county$NAICS_code[i] == "54" )
    {cbp_county$sector[i] <- "Professional, scientific, and technical services"}
  if(cbp_county$NAICS_code[i] == "55" )
    {cbp_county$sector[i] <- "Management of companies and enterprises"}
  if(cbp_county$NAICS_code[i] == "56")
    {cbp_county$sector[i] <- "Waste management and remediation services"}
  if(cbp_county$NAICS_code[i] == "61")
    {cbp_county$sector[i] <- "Educational services"}
  if(cbp_county$NAICS_code[i] == "62")
    {cbp_county$sector[i] <- "Health care and social assistance"}
  if(cbp_county$NAICS_code[i] == "71")
    {cbp_county$sector[i] <- "Arts, entertainment, and recreation"}
  if(cbp_county$NAICS_code[i] == "72")
    {cbp_county$sector[i] <- "Accommodation and food services"}
  if(cbp_county$NAICS_code[i] == "81")
    {cbp_county$sector[i] <- "Other services (except public administration)"}
  if(cbp_county$NAICS_code[i] == "99")
  {cbp_county$sector[i] <- "Industry not classified"}}

rm(i)

#sum data to condensed sectors
cbp_county %<>% group_by(year, county_name, sector) %>%
  summarize(employees = sum(employees))

#add subregional field
nj_counties <- c("Burlington", "Camden", "Gloucester", "Mercer")
cbp_county$subregion <- ifelse(cbp_county$county_name == "Philadelphia", "Philadelphia Subregion",
                                ifelse(cbp_county$county_name %in% nj_counties, "New Jersey Suburbs", 
                                       "Pennsylvania Suburbs"))
rm(nj_counties)