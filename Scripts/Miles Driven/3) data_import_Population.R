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

#import 2010-2017 population estimates
pop2010to2017 <- read_csv("../Data/Population Data/PEP_2017_PEPANNRES_with_ann.csv")[-1,] %>%
  select(-GEO.id, -rescen42010, -resbase42010) %>%
  rename(geoid = GEO.id2, county = 'GEO.display-label', 
         popEstimate2010 = respop72010, popEstimate2011 = respop72011, popEstimate2012 = respop72012, 
         popEstimate2013 = respop72013, popEstimate2014 = respop72014, popEstimate2015 = respop72015, 
         popEstimate2015 = respop72015, popEstimate2016 = respop72016, popEstimate2017 = respop72017)

#shorten county names
pop2010to2017$county %<>% str_replace(" County, New Jersey", "")
pop2010to2017$county %<>% str_replace(" County, Pennsylvania", "")

#import 2000-2009 population estimates
PA_Counties <- c("Bucks County", "Chester County", "Delaware County", 
                 "Montgomery County", "Philadelphia County")

NJ_Counties <- c("Burlington County", "Camden County", 
                 "Gloucester County", "Mercer County")

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

rm(PA_Counties, NJ_Counties)

pop2000to2009$county %<>% str_replace(" County", "")

#join tables, convert population and year variables to numeric
pop <- left_join(pop2000to2009, pop2010to2017, by = "county") %>% 
  select(geoid, everything()) %>%
  gather("year", "pop", 6:23) 

pop$year %<>% str_replace("popEstimate", "") %>%
              as.numeric()

pop$pop %<>% as.numeric()

#remove individual population data frames
rm(pop2000to2009, pop2010to2017)
  

