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

#import 2009-2016 ACS Data, filter to age 25 and older totals and gather ed categories
edAttain5yrACS <- NULL
for(i in 2009:2017){
  
  file_name <- paste("../Data/5-year data/ACS_", i, "_5YR_S1501.csv", sep = "")
  
  if(i >= 2015){
    temp <- read_csv(file_name) %>% 
            select(geoid = GEO.id2, geography = 'GEO.display-label', lessThanHS = HC01_EST_VC09, 
                   someHS = HC01_EST_VC10, hsGrad = HC01_EST_VC11, someCollege = HC01_EST_VC12, 
                   assocDegree = HC01_EST_VC13, bachDegree = HC01_EST_VC14, gradDegree = HC01_EST_VC15)}
  
  if(i>=2010 & i <=2014){
    temp <- read_csv(file_name) %>% 
            select(geoid = GEO.id2, geography = 'GEO.display-label', pop = HC01_EST_VC07, 
                   lessThanHS = HC01_EST_VC08, someHS = HC01_EST_VC09, hsGrad = HC01_EST_VC10, 
                   someCollege = HC01_EST_VC11, assocDegree = HC01_EST_VC12, 
                   bachDegree = HC01_EST_VC13, gradDegree = HC01_EST_VC14)
    for (x in c(4:10)){
      for(y in c(1:nrow(temp))){
        temp[y, x] <- temp[y, x] * (temp[y, 3] / 100)}}
    temp <- select(temp, -pop)
  }

  if(i<=2009){
    temp <- read_csv(file_name) %>% 
      select(geoid = GEO.id2, geography = 'GEO.display-label', pop = HC01_EST_VC06, 
             lessThanHS = HC01_EST_VC07, someHS = HC01_EST_VC08, hsGrad = HC01_EST_VC09, 
             someCollege = HC01_EST_VC10, assocDegree = HC01_EST_VC11, 
             bachDegree = HC01_EST_VC12, gradDegree = HC01_EST_VC13)
    for (x in c(4:10)){
      for(y in c(1:nrow(temp))){
        temp[y, x] <- temp[y, x] * (temp[y, 3] / 100)}}
    temp <- temp %>% select(-pop)}
  
  temp$year <- i
  
  for(i in c(1:nrow(temp))){
    if(is.na(temp$geoid[i])){temp$geoid[i] <- 9999999999}}
  
  temp <- temp[, c(10, 1:9)] %>% gather("edCategory", "n", 4:10)
  
  edAttain5yrACS %<>% bind_rows(temp)
  rm(file_name, temp, i, x, y)
}

#import and format 2000 longform census data
municCensus2000 <- read_csv("../Data/5-year data/DEC_2000_SF3_P037.csv") %>%
  mutate("lessThanHS" = VD03 + VD04 + VD05 + VD06 + VD20 + VD21 + VD22 + VD23,
         "someHS" = VD07 + VD08 + VD09 + VD10 + VD24 + VD25 + VD26 + VD27, 
         "hsGrad" = VD11 + VD28, 
         "someCollege" = VD12 + VD13 + VD29 + VD30, 
         "assocDegree" = VD14 + VD31, 
         "bachDegree" = VD15 + VD32, 
         "gradDegree" = VD16 + VD17 +VD18 + VD33 +VD34 + VD35) %>%
  select(geoid = GEO.id2, geography = 'GEO.display-label', lessThanHS, someHS, hsGrad, 
         someCollege, assocDegree, bachDegree, gradDegree) %>%
  gather("edCategory", "n", 3:9)

municCensus2000$year <- 2000

#add geoid for United States
for(i in c(1:nrow(municCensus2000))){
  if(is.na(municCensus2000$geoid[i])){municCensus2000$geoid[i] <- 9999999999}
  rm(i)}

#join 2000 longform to ACS
edattain5yr <- bind_rows(municCensus2000, edAttain5yrACS)
rm(municCensus2000, edAttain5yrACS)

#factor and label ed categories
edattain5yr$edCategory %<>% 
  factor(levels = c("lessThanHS", "someHS", "hsGrad", "someCollege", 
                    "assocDegree", "bachDegree", "gradDegree"), 
         labels = c("Less than High School", "Some High School",  "Graduated High School", "Some College",
                    "Associate's Degree", "Bachelor's Degree", "Graduate/Professional Degree"))

#abbreviate geopgraphy
edattain5yr$geography %<>% 
  str_replace_all(c("Pennsylvania" = "PA", 
                    "New Jersey" = "NJ", 
                    "County" = "Cty"))

#change name of Washington township (name changed to Robbinsville in 2008)
for(i in 1:length(edattain5yr$year)){
  if(edattain5yr$geoid[i] == 3402177210){
    edattain5yr$geography[i] <- "Robbinsville township, Mercer Cty, NJ"}
  if(edattain5yr$geoid[i] == 3402177210){
    edattain5yr$geoid[i] <- 3402163850}}
rm(i)

#join 2045 planning areas
planningAreas <- read_excel("../Data/2045 Planning Areas.xlsx") %>% 
                 select(geoid = GEOID, planningArea = PA_2045)

edattain5yr %<>% full_join(planningAreas, by = "geoid")

rm(planningAreas)