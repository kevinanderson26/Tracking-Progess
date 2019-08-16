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

#import 2007-2016 ACS Data, filter to age 25 and older totals and gather ed categories
edattain1yrACS <- NULL
for(i in c(2007:2017)){
  
  file_name <- paste("Raw Data/Educational Attainment/1-year data/ACS_", i, "_1YR_S1501.csv", sep = "")

  if(i >= 2015){
    temp <- read_csv(file_name) %>% 
            select(geoid = GEO.id2, geography = 'GEO.display-label', lessThanHS = HC01_EST_VC09, 
                   someHS = HC01_EST_VC10, hsGrad = HC01_EST_VC11, someCollege = HC01_EST_VC12, 
                   assocDegree = HC01_EST_VC13, bachDegree = HC01_EST_VC14, gradDegree = HC01_EST_VC15)}
  
  if(i>=2010 & i <=2014){
    temp <- read_csv(file_name) %>% 
            select(geoid = GEO.id2, geography = 'GEO.display-label', HC01_EST_VC07, lessThanHS = HC01_EST_VC08, 
                   someHS = HC01_EST_VC09, hsGrad = HC01_EST_VC10, someCollege = HC01_EST_VC11, 
                   assocDegree = HC01_EST_VC12, bachDegree = HC01_EST_VC13, gradDegree = HC01_EST_VC14)
    
    for (x in 4:10){
      for(y in 1:10){
        temp[y, x] <- temp[y, x] * (temp[y, 3] / 100)}}
    
    temp %<>% select(-HC01_EST_VC07)}
  
  if(i<=2009){
    temp <- read_csv(file_name) %>% 
            select(geoid = GEO.id2, geography = 'GEO.display-label', HC01_EST_VC06, lessThanHS = HC01_EST_VC07, 
                   someHS = HC01_EST_VC08, hsGrad = HC01_EST_VC09, someCollege = HC01_EST_VC10, 
                   assocDegree = HC01_EST_VC11, bachDegree = HC01_EST_VC12, gradDegree = HC01_EST_VC13)
    
    for (x in 4:10){
      for(y in 1:10){ 
        temp[y, x] <- temp[y, x] * (temp[y, 3] / 100)}}
      
    temp %<>% select(-HC01_EST_VC06)}
  
  temp$year <- i
  
  for(i in c(1:10)){
    if(is.na(temp$geoid[i])){
      temp$geoid[i] <- 99999}}
  
  temp <- temp[, c(10, 1:9)] %>% gather("edCategory", "n", 4:10)
  
  edattain1yrACS %<>% bind_rows(temp)
  
  rm(destination, file_name, temp, x, y, i)
}

#import and format 2000 longform census data
countyCensus2000 <- read_csv("Raw Data/Educational Attainment/1-year data/DEC_00_SF3_P037_with_ann.csv") %>% 
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

countyCensus2000$year <- 2000

#add geoid for United States
for(i in c(1:nrow(countyCensus2000))){
  if(is.na(countyCensus2000$geoid[i])){
    countyCensus2000$geoid[i] <- 99999}
  
  rm(i)}

#join 2000 longform to ACS
edattain1yr <- bind_rows(countyCensus2000, edattain1yrACS)
rm(countyCensus2000, edattain1yrACS)

#factor and label ed categories
edattain1yr$edCategory %<>% 
  factor(levels = c("lessThanHS", "someHS", "hsGrad", "someCollege", 
                    "assocDegree", "bachDegree", "gradDegree"), 
         labels = c("Less than High School", "Some High School", "Graduated High School", "Some College", 
                    "Associate's Degree", "Bachelor's Degree", "Graduate/Professional Degree"))

#abbreviate geography
edattain1yr$geography %<>% str_replace_all(c("Pennsylvania" = "", 
                                             "New Jersey" = "", 
                                             " County, " = ""))

#add subregion variable
njCounties <- c("Burlington", "Camden", "Mercer", "Gloucester")
edattain1yr$subregion <- ifelse(edattain1yr$geography %in% njCounties, "New Jersey Suburbs",
                                ifelse(edattain1yr$geography == "Philadelphia", "Philadelphia Subregion",
                                       "Pennsylvania Suburbs"))
rm(njCounties)