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

#import 2006-2017 PA and 2006-2016 NJ data
crashPA <- read_excel("../Data/Crash Data/2240_Crash_bg.xlsx", sheet = "PA")
crashNJ <- read_excel("../Data/Crash Data/2240_Crash_bg.xlsx", sheet = "NJ")

#merge PA and NJ data into single dataframe, rename variables
crashDVRPC <- bind_rows(crashPA, crashNJ) %>% 
              rename(county = 'County Name', 
                     year = 'YEAR', crashTotal = 'Total Crash', 
                     deathTotal = 'FATAL Count', 
                     majInjTotal = 'MAJ_INJ COUNT', 
                     injTotal = 'TOT_INJ COUNT', 
                     deathBicycle = 'BICYCLE DEATH COUNT', 
                     majInjBicycle = 'BICYCLE MAJ_INJ COUNT', 
                     deathPed = 'PED_DEATH COUNT', 
                     majInjPed = 'PED MAJ_INJ COUNT')

#remove individual state dataframes
rm(crashPA, crashNJ)

#convert county variable to title case and year variable to numeric
crashDVRPC$county %<>% str_to_title()
crashDVRPC$year %<>% as.numeric()


#import 2010-2017 County population data, remove unneeded variables, and rename remaining variables.
pop2010to2017 <- read_csv("../Data/Population Data/PEP_2017_PEPANNRES_with_ann.csv")[-1,] %>%
  select(-GEO.id, -rescen42010, -resbase42010) %>%
  rename(geoid = GEO.id2, county = 'GEO.display-label', 
         popEstimate2010 = respop72010, popEstimate2011 = respop72011, 
         popEstimate2012 = respop72012, popEstimate2013 = respop72013, 
         popEstimate2014 = respop72014, popEstimate2015 = respop72015, 
         popEstimate2015 = respop72015, popEstimate2016 = respop72016, 
         popEstimate2017 = respop72017)

#shorten county names in 2010-2017 population dataframe
pop2010to2017$county <- str_replace(pop2010to2017$county, " County, New Jersey", "")
pop2010to2017$county <- str_replace(pop2010to2017$county, " County, Pennsylvania", "")

#create lists of county names for filtering 2000-2009 population data
PA_Counties <- c("Bucks County", "Chester County", "Delaware County", "Montgomery County", "Philadelphia County")
NJ_Counties <- c("Burlington County", "Camden County", "Gloucester County", "Mercer County")

#import 2000-2009 County population data, filter to DVRPC counties, 
#remove unneeded variables, and rename remaining variables.
pop2000to2009 <- read_csv("../Data/Population Data/co-est00int-tot.csv")[,-c(1:3)] %>% 
  filter((STNAME == "Pennsylvania"  & CTYNAME  %in% PA_Counties) | 
           (STNAME == "New Jersey" & CTYNAME %in% NJ_Counties)) %>%
  select(-ESTIMATESBASE2000, -CENSUS2010POP, -POPESTIMATE2010) %>%
  rename(stateCode = STATE, countyCode = COUNTY, state = STNAME, county = CTYNAME, 
         popEstimate2000 = POPESTIMATE2000, popEstimate2001 = POPESTIMATE2001, 
         popEstimate2002 = POPESTIMATE2002, popEstimate2003 = POPESTIMATE2003, 
         popEstimate2004 = POPESTIMATE2004, popEstimate2005 = POPESTIMATE2005, 
         popEstimate2006 = POPESTIMATE2006, popEstimate2007 = POPESTIMATE2007, 
         popEstimate2008 = POPESTIMATE2008, popEstimate2009 = POPESTIMATE2009)

#remove lists of county names
rm(PA_Counties, NJ_Counties)

#shorten county names in 2000-2009 population dataframe
pop2000to2009$county <- str_replace(pop2000to2009$county, " County", "")

#join population dataframes, move geoid to front, gather remaining columns to year and population variables
pop <- left_join(pop2000to2009, pop2010to2017, by = "county") %>% 
  select(geoid, everything()) %>%
  gather("year", "pop", 6:23) 

#convert year and population variables from strings to numeric
pop$year <- as.numeric(str_replace(pop$year, "popEstimate", ""))
pop$pop <- as.numeric(pop$pop)

#remove individual population data frames
rm(pop2000to2009, pop2010to2017)

#import County VMT data, gather and rename variables, convert daily VMT into annual VMT
vmt <- read_excel("../Data/VMT Data/VMT.xlsx") %>%
  gather(County, milesDaily, -Year) %>%
  rename(year = Year, county = County) %>%
  mutate(milesAnnual = milesDaily * 365)


#merge pop and vmt data to crash data
crashAll <- left_join(crashDVRPC, pop, by = c("county", "year")) %>%
  left_join(vmt, by = c("county", "year")) %>%
  select(stateCode, countyCode, geoid, state, everything())

#replace all missing values with zero
crashAll[is.na(crashAll)] <- 0

#remove individual dataframes for crashes, population, and vmt
rm(crashDVRPC, pop, vmt)

#create NJ county list for subregion filtering, add subregion variable to crashAll dataframe
njCounties <- c("Burlington", "Camden", "Mercer", "Gloucester")
crashAll$subregion <- ifelse(crashAll$county %in% njCounties, "NJ Suburbs",
                              ifelse(crashAll$county =="Philadelphia", "Philadelphia", "PA Suburbs"))

#remove NJ county list
rm(njCounties)