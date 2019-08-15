#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse")
pack(packages)
rm(pack, packages)

#import raw data for each pollutant, join into single df, 

aqOzone <- NULL
for(i in 1980:2018){
  path <- paste("Raw Data/Air Quality/daily/aqidaily", i, "ozone", ".csv", sep = "")
  destination <- paste("aqidaily", i, "ozone", sep = "")
  temp <- read_csv(path)
  names(temp) <- c("date", "aqiCategoryOzone", "aqiValueOzone")
  aqOzone <- rbind(aqOzone, temp)
  rm(path, destination, temp, i)}

aqPM <- NULL
for(i in 1999:2018){
  path <- paste("Raw Data/Air Quality/daily/aqidaily", i, "pm", ".csv", sep = "")
  destination <- paste("aqidaily", i, "pm", sep = "")
  temp <- read_csv(path)
  names(temp) <- c("date", "aqiCategoryPM", "aqiValuePM")
  aqPM <- rbind(aqPM, temp)
  rm(path, destination, temp, i)}

aqiDaily <- full_join(aqOzone, aqPM, by = "date")
rm(aqOzone, aqPM)

#factor AQI categories
aqiDaily$date <- mdy(aqiDaily$date)
cat_levels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy")
aqiDaily$aqiCategoryOzone <- factor(aqiDaily$aqiCategoryOzone, levels = cat_levels)
aqiDaily$aqiCategoryPM <- factor(aqiDaily$aqiCategoryPM, levels = cat_levels)
rm(cat_levels)

#add year, month, and quarter columns
aqiDaily$year <- floor_date(aqiDaily$date, "year")
aqiDaily$monthYear <- floor_date(aqiDaily$date, "month")
aqiDaily$quarterYear <- floor_date(aqiDaily$date, "quarter")