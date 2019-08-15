#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "sf", "tidycensus", 
              "readxl", "units", "scales", "measurements")
pack(packages)
rm(pack, packages)
options(scipen=999)

#download deliminated data for 2010-2018 to disk (only uncomment if need to redownload)
#update years_1 range to add new year of data

#years_1 <- seq(2010, 2018, 1) %>% as.character()
#
#for(i in 1:length(years_1)){
#  file_name_PA <- paste("https://www.fhwa.dot.gov/bridge/nbi/", years_1[i], "/delimited/PA", 
#                     str_sub(years_1[i], 3, 4), ".txt", sep = "")
#  destination_PA <- paste("../Data/Pennsylvania/PA", str_sub(years_1[i], 3, 4), ".txt", sep = "")
#  variable_PA <- paste("bridges", "PA", years_1[i], sep ="_")
#  
#  file_name_NJ <- paste("https://www.fhwa.dot.gov/bridge/nbi/", years_1[i], "/delimited/NJ", 
#                        str_sub(years_1[i], 3, 4), ".txt", sep = "")
#  destination_NJ <- paste("../Data/New Jersey/NJ", str_sub(years_1[i], 3, 4), ".txt", sep = "")
#  variable_NJ <- paste("bridges", "NJ", years_1[i], sep ="_")
#}


#download zipped deliminated data for 2000-2009, extra PA and NJ and save to disk (only uncomment if need to redownload)
#years_2 <- seq(2000, 2009, 1) %>% as.character()

#for(i in 1:length(years_2)){
#  download .zip file to temp location, extract data
#  temp <- tempfile()
#  file_name <- paste("https://www.fhwa.dot.gov/bridge/nbi/", years_2[i], "hwybronlyonefile.zip", sep = "")
#  download.file(file_name, temp)
#  data <- read_delim(unzip(temp), delim = ",")
#  unlink(temp)
  
  #filter extracted data for only Pennsylvania and New Jersey
#  pa_data <- data %>%
#    filter(STATE_CODE_001 == "42")
#  nj_data <- data %>%
#    filter(STATE_CODE_001 == "34")
#  
  #save pa and nj data to disk
#  destination_PA <- paste("../Data/Pennsylvania/PA", str_sub(years_2[i], 3, 4), ".txt", sep = "")
#  destination_NJ <- paste("../Data/New Jersey/NJ", str_sub(years_2[i], 3, 4), ".txt", sep = "")
#  write_delim(pa_data, destination_PA, delim = ",")
#  write_delim(nj_data, destination_NJ, delim = ",")
#}

###import data into R and bind

#define years range
#update years_3 range to add new years of data
years_3 <- seq(2000, 2018, 1) %>% as.character()

#create empty data frame
bridges_all <- data.frame()

#loop over years_3 to import each year of data
#loop selects only required variables for each state, merges the selected vehicle, and then adds to the empty data frame
for(i in 1:length(years_3)){
  filename_PA <- paste("Raw Data/Bridge Conditions/Pennsylvania/PA", str_sub(years_3[i], 3, 4), ".txt", sep = "")
  filename_NJ <- paste("Raw Data/Bridge Conditions/New Jersey/NJ", str_sub(years_3[i], 3, 4), ".txt", sep = "")
  
  pa_temp <- read_delim(filename_PA, delim = ",", quote = "\'", escape_double = FALSE,
                        col_types = cols(.default = "c")) %>%
    select(facility_carried = FACILITY_CARRIED_007,
           features = FEATURES_DESC_006A,
           location = LOCATION_009,
           state = STATE_CODE_001,
           county = COUNTY_CODE_003,
           latitude = LAT_016,
           longitude = LONG_017,
           maintainer_code = MAINTENANCE_021,
           owner_code = OWNER_022,
           structure_number = STRUCTURE_NUMBER_008,
           length_m = STRUCTURE_LEN_MT_049,
           width_m = DECK_WIDTH_MT_052,
           approach_width_m = APPR_WIDTH_MT_032,
           rating_deck = DECK_COND_058,
           rating_super = SUPERSTRUCTURE_COND_059,
           rating_sub = SUBSTRUCTURE_COND_060,
           rating_culvert = CULVERT_COND_062
           ) %>%
    mutate(year = as.numeric(years_3[i]))
    
  nj_temp <- read_delim(filename_NJ, delim = ",", quote = "\'", escape_double = FALSE,
                        col_types = cols(.default = "c")) %>%
    select(facility_carried = FACILITY_CARRIED_007,
           features = FEATURES_DESC_006A,
           location = LOCATION_009,
           state = STATE_CODE_001,
           county = COUNTY_CODE_003,
           latitude = LAT_016,
           longitude = LONG_017,
           maintainer_code = MAINTENANCE_021,
           owner_code = OWNER_022,
           structure_number = STRUCTURE_NUMBER_008,
           length_m = STRUCTURE_LEN_MT_049,
           width_m = DECK_WIDTH_MT_052,
           approach_width_m = APPR_WIDTH_MT_032,
           rating_deck = DECK_COND_058,
           rating_super = SUPERSTRUCTURE_COND_059,
           rating_sub = SUBSTRUCTURE_COND_060,
           rating_culvert = CULVERT_COND_062) %>%
    mutate(year = as.numeric(years_3[i]))

  bridges_all %<>% bind_rows(pa_temp, nj_temp)}

#remove unneeded temp files
rm(pa_temp, nj_temp, i, filename_PA, filename_NJ, years_3)

#replace "N" ratings with 100, convert to numeric
#100 now means Not Applicable, and NA is missing
#when filtering for deficient bridges, 100 is above all relevant thresholds
bridges_all %<>%
  mutate(rating_deck = replace(rating_deck, rating_deck == "N", 100),
         rating_super = replace(rating_super, rating_super == "N", 100), 
         rating_sub = replace(rating_sub, rating_sub == "N", 100),
         rating_culvert = replace(rating_culvert, rating_culvert == "N", 100)) %>%
  mutate(rating_deck = as.numeric(rating_deck),
         rating_super = as.numeric(rating_super), 
         rating_sub = as.numeric(rating_sub),
         rating_culvert = as.numeric(rating_culvert))



#create state+county id, filter bridges to just DVRPC
bridges_all$state_county <- paste(bridges_all$state, bridges_all$county, sep = "")

county_list <- c("34005", "34007", "34015", "34021",
                 "42017", "42029", "42045", "42091", "42101")

bridges_region <- bridges_all %>%
  filter(state_county %in% county_list)

rm(county_list)

#add subregion field
pa_suburbs <- c("42017", "42029", "42045", "42091")
nj_suburbs <- c("34005", "34007", "34015", "34021")

for(i in 1:length(bridges_region$state_county)){
bridges_region$subregion[i] <- ifelse(bridges_region$state_county[i] %in% pa_suburbs, "Pennsylvania Suburbs",
                                      ifelse(bridges_region$state_county[i] %in% nj_suburbs, "New Jersey Suburbs",
                                             "Philadelphia"))}

rm(i, pa_suburbs, nj_suburbs)

#add state/local maintainer category
#all codes not in state_main or local_main will get grouped into "other" 
#NBI data dictionary with codes is at http://nationalbridges.com/nbiDesc.html#ITEM_58
state_main <- c("01", "11", "21")
local_main <- c("02", "03", "04")

bridges_region %<>%
  mutate(maintainer_class = 
           ifelse(maintainer_code %in% state_main, "State",
                  ifelse(maintainer_code %in% local_main, "Local", "Other")),
         owner_class = 
           ifelse(owner_code %in% state_main, "State",
                  ifelse(owner_code %in% local_main, "Local", "Other")))

bridges_region$maintainer_class %<>% as_factor() %>% fct_relevel("Other", "Local", "State")

rm(state_main, local_main)

#change state and county numeric codes to text
bridges_region %<>%
  mutate(state = ifelse(state == "34", "New Jersey", "Pennsylvania"),
         county = ifelse(county == "017", "Bucks", 
                    ifelse(county == "029", "Chester",  
                      ifelse(county == "045" , "Delaware", 
                        ifelse(county == "091", "Montgomery",  
                          ifelse(county == "101", "Philadelphia",  
                            ifelse(county == "007", "Camden",  
                              ifelse(county == "005", "Burlington",  
                                ifelse(county == "015", "Gloucester",  
                                  "Mercer")))))))))

#convert lengths/widths to numeric
bridges_region %<>% mutate(length_m = as.numeric(length_m),
                        width_m = as.numeric(width_m),
                        approach_width_m = as.numeric(approach_width_m))

#convert lengths/widths from meters to feet, add area field
#area is calculated using bridge width, if it exists. If bridge with is zero, approach width is used instead
bridges_region %<>%
  mutate(length_ft = length_m * 3.28084,
         width_ft = width_m * 3.28084,
         approach_width_ft = approach_width_m* 3.28084,
         area_sqft = ifelse(width_ft != 0, length_ft * width_ft, length_ft * approach_width_ft))

#reorder variables, drop measurements in meters
bridges_region %<>%
  select(facility_carried, features, location, state, county, state_county, subregion, latitude, longitude,
         maintainer_code, maintainer_class,
         owner_code, owner_class,
         structure_number, year, length_ft, width_ft, approach_width_ft, area_sqft,
         rating_deck, rating_super, rating_sub, rating_culvert)