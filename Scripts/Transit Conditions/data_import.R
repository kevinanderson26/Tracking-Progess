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

agencyCodes <- c(2075, 2080, 3019, 3077)

vehicleAge2017 <- read_excel("../Data/2017 Age Distribution.xlsm", sheet = "Age Distribution") %>%
  select(agencyCode = 'Legacy NTD ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0 = '0', age_1 = '1', age_2 = '2', age_3 = '3', age_4 = '4', age_5 = '5', age_6 = '6',
         age_7 = '7', age_8 = '8', age_9 = '9', age_10 = '10', age_11 = '11', age_12 = '12', 
         age_13_15 = '13-15', age_16_20 = '16-20', age_21_25 = '21-25', age_26_30 = '26-30',
         age_31_60 = '31-60', age_60over = '60+') %>%
  mutate(age_0_5 = age_0 + age_1 + age_2 + age_3 + age_4 + age_5,
         age_6_11 = age_6 + age_7 + age_8 + age_9 + age_10 + age_11,
         age_12_15 = age_12 + age_13_15,
         age_25over = age_26_30 + age_31_60 + age_60over) %>%
  select(agencyCode, vehicle, nVehicles, avgAge, 
         age_0_5, age_6_11, age_12_15, 
         age_16_20,age_21_25, age_25over) %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2017)

vehicleAge2016 <- read_excel("../Data/2016 Age Distribution.xlsm", sheet = "Age Distribution") %>%
  select(agencyCode = 'Legacy NTD ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0 = '0', age_1 = '1', age_2 = '2', age_3 = '3', age_4 = '4', age_5 = '5', age_6 = '6',
         age_7 = '7', age_8 = '8', age_9 = '9', age_10 = '10', age_11 = '11', age_12 = '12', 
         age_13_15 = '13-15', age_16_20 = '16-20', age_21_25 = '21-25', age_26_30 = '26-30',
         age_31_60 = '31-60', age_60over = '60+') %>%
  mutate(age_0_5 = age_0 + age_1 + age_2 + age_3 + age_4 + age_5,
         age_6_11 = age_6 + age_7 + age_8 + age_9 + age_10 + age_11,
         age_12_15 = age_12 + age_13_15,
         age_25over = age_26_30 + age_31_60 + age_60over) %>%
  select(agencyCode, vehicle, nVehicles, avgAge, 
         age_0_5, age_6_11, age_12_15, 
         age_16_20,age_21_25, age_25over) %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2016)

vehicleAge2015 <- read_excel("../Data/2015 Age Distribution.xlsm", sheet = "Age Distribution") %>%
  select(agencyCode = 'Legacy NTD ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0 = '0', age_1 = '1', age_2 = '2', age_3 = '3', age_4 = '4', age_5 = '5', age_6 = '6',
         age_7 = '7', age_8 = '8', age_9 = '9', age_10 = '10', age_11 = '11', age_12 = '12', 
         age_13_15 = '13-15', age_16_20 = '16-20', age_21_25 = '21-25', age_26_30 = '26-30',
         age_31_60 = '31-60', age_60over = '60+') %>%
  mutate(age_0_5 = age_0 + age_1 + age_2 + age_3 + age_4 + age_5,
         age_6_11 = age_6 + age_7 + age_8 + age_9 + age_10 + age_11,
         age_12_15 = age_12 + age_13_15,
         age_25over = age_26_30 + age_31_60 + age_60over) %>%
  select(agencyCode, vehicle, nVehicles, avgAge, 
         age_0_5, age_6_11, age_12_15, 
         age_16_20,age_21_25, age_25over) %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2015)

vehicleAge2014 <- read_excel("../Data/2014 Age Distribution.xls") %>%
  select(agencyCode = 'Legacy NTDID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2014)

vehicleAge2013 <- read_excel("../Data/2013 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2013)

vehicleAge2012 <- read_excel("../Data/2012 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2012)

vehicleAge2011 <- read_excel("../Data/2011 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2011)

vehicleAge2010 <- read_excel("../Data/2010 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2010)

vehicleAge2009 <- read_excel("../Data/2009 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2009)

vehicleAge2008 <- read_excel("../Data/2008 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of fleet (in years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2008)

vehicleAge2007 <- read_excel("../Data/2007 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet', avgAge = 'Average Age of Fleet (in years)',
         age_0_5 = '5 or Less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2007)

#fleet size and average age columns are flipped in 2006 table from NTD
vehicleAge2006 <- read_excel("../Data/2006 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Average Age of Fleet (in Years)', avgAge = 'Total Active Fleet',
         age_0_5 = '5 or less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2006)

vehicleAge2005 <- read_excel("../Data/2005 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet*', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2005)

vehicleAge2004 <- read_excel("../Data/2004 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet*', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2004)

vehicleAge2003 <- read_excel("../Data/2003 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet*', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2003)

vehicleAge2002 <- read_excel("../Data/2002 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet*', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or less', age_6_11 = '6 to 11', age_12_15 = '12 to 15',
         age_16_20 = '16 to 20', age_21_25 = '21 to 25', age_25over = 'Over 25') %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2002)

#2001 and prior need to fill agency code columns, remove suffix from codes
vehicleAge2001 <- read_excel("../Data/2001 Age Distribution.xlsx") %>%
  select(agencyCode = 'ID/ Org', vehicle = 'Vehicle Type', 
         nVehicles = 'Total Active Fleet*', avgAge = 'Average Age of Fleet (in Years)',
         age_0_5 = '5 or less', age_6_11 = '6-11', age_12_15 = '12-15',
         age_16_20 = '16-20', age_21_25 = '21-25', age_25over = 'Over 25') %>%
  filter(!is.na(vehicle)) %>%
  fill(agencyCode, .direction = "down") %>%
  mutate(agencyCode = str_replace(agencyCode, "-[:alpha:]", "")) %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2001)

vehicleAge2000 <- read_excel("../Data/2000 Age Distribution.xls") %>%
select(agencyCode = 'ID/Org', vehicle = 'Vehicle Type', 
       nVehicles = 'Total Active Fleet*', avgAge = 'Average Age of Fleet (in Years)',
       age_0_5 = '5 or Less', age_6_11 = '6-11', age_12_15 = '12-15',
       age_16_20 = '16-20', age_21_25 = '21-25', age_25over = 'Over 25') %>%
  filter(!is.na(vehicle)) %>%
  fill(agencyCode, .direction = "down") %>%
  mutate(agencyCode = str_replace(agencyCode, "-[:alpha:]", "")) %>%
  filter(agencyCode %in% agencyCodes) %>%
  mutate(year = 2000)

#bind individual years into single table, fill NAs with zeros
vehicleAge <- bind_rows(vehicleAge2000, vehicleAge2001, vehicleAge2002, 
                        vehicleAge2003, vehicleAge2004, vehicleAge2005, 
                        vehicleAge2006, vehicleAge2007, vehicleAge2008, 
                        vehicleAge2009, vehicleAge2010, vehicleAge2011, 
                        vehicleAge2012, vehicleAge2013, vehicleAge2014, 
                        vehicleAge2015, vehicleAge2016, vehicleAge2017) %>% 
              replace(is.na(.), 0)


rm(agencyCodes, vehicleAge2000, vehicleAge2001, vehicleAge2002, vehicleAge2003, 
   vehicleAge2004, vehicleAge2005, vehicleAge2006, vehicleAge2007, vehicleAge2008, 
   vehicleAge2009, vehicleAge2010, vehicleAge2011, vehicleAge2012, vehicleAge2013, 
   vehicleAge2014, vehicleAge2015, vehicleAge2016, vehicleAge2017)

#add agency names to table
name_table <- data.frame(agencyCode = c("2080", "2075", "3019", "3077"),
                         agency_name = c("NJ Transit", "PATCO", "SEPTA", "PART"))

vehicleAge %<>% left_join(name_table, by = "agencyCode")

rm(name_table)
