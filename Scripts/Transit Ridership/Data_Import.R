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

agencyCodes <- c(2075, 2080, 3019, 3077)
uzaCodes <- c(5, 128, 287)

#import individual years
unlinkedTrips1997 <- read_excel("../Data/1997_Database_406.xlsx") %>%
  select(agencyCode = TRS_ID, mode = MODE, 
         service = SERVICE, unlinkedTrips = iUNLNKTRIP) %>%
  mutate(year = 1997) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips1998 <- read_excel("../Data/1998_Database_406.xlsx") %>%
  select(agencyCode = TRS_ID, mode = MODE, 
         service = SERVICE, unlinkedTrips = iUNLNKTRIP) %>%
  mutate(year = 1998) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips1999 <- read_excel("../Data/1999_Database_406.xlsx") %>%
  select(agencyCode = TRS_ID, mode = MODE, 
         service = SERVICE, unlinkedTrips = iUNLNKTRIP) %>%
  mutate(year = 1999) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2000 <- read_excel("../Data/2000_Database_406.xlsx") %>%
  select(agencyCode = TRS_ID, mode = MODE, 
         service = SERVICE, unlinkedTrips = iUNLNKTRIP) %>%
  mutate(year = 2000) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2001 <- read_excel("../Data/2001_Database_406.xlsx") %>%
  select(agencyCode = TRS_ID, mode = MODE, 
         service = SERVICE, unlinkedTrips = iUNLNKTRIP) %>%
  mutate(year = 2001) %>%
  filter(agencyCode %in% agencyCodes)
 
unlinkedTrips2002 <- read_excel("../Data/2002_Service.xlsx") %>%
  filter(TIME_PERIOD_DESC == "Annual Total") %>%
  select(agencyCode = TRS_ID, mode = MODE_CD, 
         service = SERVICE_CD, unlinkedTrips = UNL_PASS_TRIPS_NUM) %>%
  mutate(year = 2002) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2003 <- read_excel("../Data/2003_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2003) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2004 <- read_excel("../Data/2004_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2004) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2005 <- read_excel("../Data/2005_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2005) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2006 <- read_excel("../Data/2006_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2006) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2007 <- read_excel("../Data/2007_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2007) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2008 <- read_excel("../Data/2008_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2008) %>%
  filter(agencyCode %in% agencyCodes)  

unlinkedTrips2009 <- read_excel("../Data/2009_Service.xlsx") %>%
filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2009) %>%
  filter(agencyCode %in% agencyCodes) 

unlinkedTrips2010 <- read_excel("../Data/2010_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2010) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2011 <- read_excel("../Data/2011_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unlinked_Passenger_Trips) %>%
  mutate(year = 2011) %>%
  filter(agencyCode %in% agencyCodes)

unlinkedTrips2012 <- read_excel("../Data/2012_Service.xlsx") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  select(agencyCode = Trs_Id, mode = Mode_Cd, 
         service = Service_Cd, unlinkedTrips = Unl_Pass_Trips_Num) %>%
  mutate(year = 2012) %>%
  filter(agencyCode %in% agencyCodes) 

unlinkedTrips2013 <- read_excel("../Data/2013_Federal_Funding_Allocation.xlsx") %>%
  select(uzaCode = UZA, uzaName = 'Urbanized Area Name', agencyCode = 'NTDID', 
         mode = Mode, service = TOS, 
         unlinkedTrips = 'Annual Total Unlinked Passenger Trips') %>%
  mutate(year = 2013) %>%
  filter(agencyCode %in% agencyCodes &
           uzaCode %in% uzaCodes) %>%
  group_by(agencyCode, mode, service, year) %>%
  summarize(unlinkedTrips = sum(unlinkedTrips)) %>%
  ungroup()

unlinkedTrips2014 <- read_excel("../Data/2014_Federal_Funding_Allocation.xlsx", skip = 1) %>%
select(uzaCode = UZA, uzaName = 'UZA Name', agencyCode = '4 digit NTDID', 
       mode = Mode, service = TOS, 
       unlinkedTrips = 'Annual Unlinked Passenger Trips') %>%
  mutate(year = 2014) %>%
  filter(agencyCode %in% agencyCodes &
           uzaCode %in% uzaCodes) %>%
  group_by(agencyCode, mode, service, year) %>%
  summarize(unlinkedTrips = sum(unlinkedTrips)) %>%
  ungroup()

unlinkedTrips2015 <- read_excel("../Data/2015_Federal_Funding_Allocation.xlsx") %>%
  select(uzaCode = UZA, uzaName = 'UZA Name', agencyCode = 'Legacy NTD ID', 
         mode = Mode, service = TOS, 
         unlinkedTrips = 'Total Unlinked Passenger Trips') %>%
  mutate(year = 2015) %>%
  filter(agencyCode %in% agencyCodes &
         uzaCode %in% uzaCodes) %>%
  group_by(agencyCode, mode, service, year) %>%
  summarize(unlinkedTrips = sum(unlinkedTrips)) %>%
  ungroup()

unlinkedTrips2016 <- read_excel("../Data/2016_Federal_Funding_Allocation.xlsx") %>%
  select(uzaCode = UZA, uzaName = 'UZA Name', agencyCode = 'Legacy NTD ID', 
         mode = Mode, service = TOS, 
         unlinkedTrips = 'Total Unlinked Passenger Trips') %>%
  mutate(year = 2016) %>%
  filter(agencyCode %in% agencyCodes &
           uzaCode %in% uzaCodes) %>%
  group_by(agencyCode, mode, service, year) %>%
  summarize(unlinkedTrips = sum(unlinkedTrips)) %>%
  ungroup()

unlinkedTrips2017 <- read_excel("../Data/2017_Federal_Funding_Allocation.xlsx") %>%
  select(uzaCode = UZA, uzaName = 'UZA Name', agencyCode = 'Legacy NTD ID', 
         mode = Mode, service = TOS, 
         unlinkedTrips = 'Total Unlinked Passenger Trips') %>%
  mutate(year = 2017) %>%
  filter(agencyCode %in% agencyCodes &
           uzaCode %in% uzaCodes) %>%
  group_by(agencyCode, mode, service, year) %>%
  summarize(unlinkedTrips = sum(unlinkedTrips)) %>%
  ungroup()

#coerce columns with incorrect data types
unlinkedTrips2000$unlinkedTrips %<>% as.numeric()
unlinkedTrips2003$agencyCode %<>% as.character()
unlinkedTrips2004$agencyCode %<>% as.character()
unlinkedTrips2006$agencyCode %<>% as.character()

#remove unneeded variables
rm(agencyCodes, uzaCodes)

#import pre-2011 PART data
part <- read_excel("../Data/PART 2008-2010.xlsx")
part$year <- year(as.Date(part$Month))
partYear <- part %>% group_by(year) %>% 
  summarize(agencyCode = "3077", 
            DR = sum(paratransit), 
            MB =sum(Bus)) %>%
  gather("mode", "unlinkedTrips", 3:4)

#bind individual year tables
unlinkedTrips <- bind_rows(unlinkedTrips1997, unlinkedTrips1998, unlinkedTrips1999, unlinkedTrips2000,
                           unlinkedTrips2001, unlinkedTrips2002, unlinkedTrips2003, unlinkedTrips2004, 
                           unlinkedTrips2005, unlinkedTrips2006, unlinkedTrips2007, unlinkedTrips2008, 
                           unlinkedTrips2009, unlinkedTrips2010, unlinkedTrips2011, unlinkedTrips2012, 
                           unlinkedTrips2013, unlinkedTrips2014, unlinkedTrips2015, unlinkedTrips2016, 
                           unlinkedTrips2017, partYear)

rm(unlinkedTrips1997, unlinkedTrips1998, unlinkedTrips1999, unlinkedTrips2000, unlinkedTrips2001, 
   unlinkedTrips2002, unlinkedTrips2003, unlinkedTrips2004, unlinkedTrips2005, unlinkedTrips2006, 
   unlinkedTrips2007, unlinkedTrips2008, unlinkedTrips2009, unlinkedTrips2010, unlinkedTrips2011, 
   unlinkedTrips2012, unlinkedTrips2013, unlinkedTrips2014, unlinkedTrips2015, unlinkedTrips2016, 
   unlinkedTrips2017, part, partYear)

#add agency and mode names
mode_names <- data.frame(code = c("CR", "DR", "HR", "LR", 
                                  "MB", "TB", "VP", "YR"),
                         mode_name = c("Commuter Rail", "Demand Response", "Subway", "Light Rail", 
                                       "Bus", "Trolleybus", "Vanpool", "Hybrid Rail"),
                         stringsAsFactors = FALSE)

unlinkedTrips %<>% left_join(mode_names, by = c("mode" = "code"))

agency_names <- data.frame(code = c("2075", "2080", "3019", "3077"),
                           agency_name = c("PATCO", "New Jersey Transit", "SEPTA", "Pottstown Area Rapid Transit"),
                           stringsAsFactors = FALSE)

unlinkedTrips %<>% left_join(agency_names, by = c("agencyCode" = "code"))

rm(mode_names, agency_names)

