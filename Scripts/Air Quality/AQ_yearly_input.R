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
aqiYearlyRaw <- NULL

for(i in 1980:2018){
  path <- paste("Raw Data/Air Quality/yearly/annual_aqi_by_cbsa_", i, ".csv", sep = "")
  temp <- read_csv(path)
  aqiYearlyRaw <- bind_rows(aqiYearlyRaw, temp)
  rm(path, temp, i)
}

#filter only for Philly CBSA, drop pollutant-specific variables
aqiYearly <- filter(aqiYearlyRaw, CBSA == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")
aqiYearly <- aqiYearly[3:13]
colnames(aqiYearly) <- c("year", "daysAQI", "daysGood", "daysModerate", "daysUnhealthySensitive", "daysUnhealthy", 
                         "daysVeryUnhealthy", "daysHazardous", "aqiMax", "aqi90Percentile", "aqimedian")

rm(aqiYearlyRaw)