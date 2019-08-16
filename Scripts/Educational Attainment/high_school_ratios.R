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

#source import files
source("Scripts/Educational Attainment/import_and_organize_1yr.R")
source("Scripts/Educational Attainment/import_and_organize_5yr.R")

#calculate percent of pop with at least high school diploma for each county

catgroup <-c("Graduated High School", "Some College", "Associate's Degree", 
             "Bachelor's Degree", "Graduate/Professional Degree")

highSchoolPlusCounty <- edattain1yr %>% filter(edCategory %in% catgroup) %>%
  group_by(geography, year) %>% 
  summarize(subregion = first(subregion), 
            highSchoolPlus = sum(n), 
            geoid = as.character(first(geoid)))

highSchoolLessCounty <- edattain1yr %>% filter(!edCategory %in% catgroup) %>% 
  group_by(geography, year) %>% 
  summarize(highSchoolLess = sum(n))

highSchoolRatioCounty <- full_join(highSchoolPlusCounty, highSchoolLessCounty, by = c("geography", "year")) %>%
  mutate(postHighSchool = (highSchoolPlus / (highSchoolPlus + highSchoolLess))) %>%
  select(year, geoid, geography, subregion, everything())

rm(highSchoolPlusCounty, highSchoolLessCounty)

#calculate percent of pop with at least high school diploma for each planning area
highSchoolPlusPA <- edattain5yr %>% filter(edCategory %in% catgroup) %>%
  group_by(planningArea, year) %>% 
  summarize(highSchoolPlus = sum(n))

highSchoolLessPA <- edattain5yr %>% filter(!edCategory %in% catgroup) %>% 
  group_by(planningArea, year) %>% 
  summarize(highSchoolLess = sum(n))

highSchoolRatioPA <- full_join(highSchoolPlusPA, highSchoolLessPA, by = c("planningArea", "year")) %>%
  mutate(postHighSchool = (highSchoolPlus / (highSchoolPlus + highSchoolLess))) %>%
  select(year, planningArea, everything())
rm(highSchoolPlusPA, highSchoolLessPA)

#calculate percent of pop with at least high school diploma for each municipality

highSchoolPlusMunic <- edattain5yr %>% filter(edCategory %in% catgroup) %>%
  group_by(geography, year) %>% 
  summarize(highSchoolPlus = sum(n), 
            geoid = as.character(first(geoid)),
            planningArea = first(planningArea))

highSchoolLessMunic <- edattain5yr %>% filter(!edCategory %in% catgroup) %>% 
  group_by(geography, year) %>% 
  summarize(highSchoolLess = sum(n))

highSchoolRatioMunic <- full_join(highSchoolPlusMunic, highSchoolLessMunic, by = c("geography", "year")) %>%
  mutate(postHighSchool = (highSchoolPlus / (highSchoolPlus + highSchoolLess))) %>%
  select(year, geoid, planningArea, geography, everything())
rm(catgroup, highSchoolPlusMunic, highSchoolLessMunic)

