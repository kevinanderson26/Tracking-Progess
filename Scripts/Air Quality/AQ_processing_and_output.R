#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "magrittr")
pack(packages)
rm(pack, packages)

#source input files
source("AQ_daily_input.R")
source("AQ_yearly_input.R")

#calculate violating days for yearly data
aqiYearly$daysViolating <- (aqiYearly$daysUnhealthySensitive +
                            aqiYearly$daysUnhealthy +
                            aqiYearly$daysVeryUnhealthy +
                            aqiYearly$daysHazardous)

#calculate 5-year moving average for yearly data
aqiYearly$fiveYearAvg <- NA
for(i in 1:length(aqiYearly$year)){
  aqiYearly$fiveYearAvg[i] <- ifelse(i <= 4 , NA, (sum(aqiYearly$daysViolating[(i-4):(i)])/5))
  rm(i)
}

#select year, violating days, and moving average variables, round data, filter to only 2000 and later, and write processed yearly data to csv
aqiYearlyFiltered <- aqiYearly %>% select(year, daysViolating, fiveYearAvg)
aqiYearlyFiltered$daysViolating <- round(aqiYearlyFiltered$daysViolating, 0)
aqiYearlyFiltered$fiveYearAvg <- round(aqiYearlyFiltered$fiveYearAvg, 0)

aqiYearlyFiltered %<>% filter(year >= 2000)

write_csv(aqiYearlyFiltered, "../Processed Data/aq_yearly.csv")

#select quarter/year, and AQI categories for daily data, group by quarter, count AQI categories
aqiQuarterlyOzone <- aqiDaily %>% select(quarterYear, aqiCategoryOzone) %>%
                      group_by(quarterYear, aqiCategoryOzone) %>%
                      summarize(nLevelOzone = n()) %>%
                      spread(aqiCategoryOzone, nLevelOzone, fill = 0) %>%
                      rename (goodOzone = Good, moderateOzone = Moderate, 
                              unhealthySensitiveOzone = 'Unhealthy for Sensitive Groups', 
                              unhealthyOzone = Unhealthy, veryUnhealthyOzone = 'Very Unhealthy')

aqiQuarterlyPM <- aqiDaily %>% select(quarterYear, aqiCategoryPM) %>%
                      group_by(quarterYear, aqiCategoryPM) %>%
                      summarize(nLevelPM = n()) %>%
                      na.omit() %>%
                      spread(aqiCategoryPM, nLevelPM, fill = 0) %>%
                      rename (goodPM = Good, moderatePM = Moderate, 
                              unhealthySensitivePM = 'Unhealthy for Sensitive Groups', 
                              unhealthyPM = Unhealthy, veryUnhealthyPM = 'Very Unhealthy')

#join summarized aqi tables for each pollutant
aqiQuarterly <- full_join(aqiQuarterlyOzone, aqiQuarterlyPM, by = "quarterYear")
rm(aqiQuarterlyOzone, aqiQuarterlyPM)

#select quarter/year and unhealthy aqi categories, filter to only 2000 and later
aqiQuarterFiltered <- aqiQuarterly %>% 
  filter(quarterYear >= "2000-01-01") %>%
  select(quarterYear, unhealthySensitiveOzone, unhealthyOzone, veryUnhealthyOzone, 
         unhealthySensitivePM, unhealthyPM, veryUnhealthyPM)

#add text quarter labels to quarter data frame
aqiQuarterFiltered %<>% 
  mutate(date_label = as.character(quarterYear),
         date_label = str_sub(date_label, start = 1, end = 7),
         date_label = str_replace(date_label, "-01", " Q1"),
         date_label = str_replace(date_label, "-04", " Q2"),
         date_label = str_replace(date_label, "-07", " Q3"),
         date_label = str_replace(date_label, "-10", " Q4")) %>%
  select(quarterYear, date_label, everything())
  
#write quarter data to csv
write_csv(aqiQuarterFiltered, "../Processed Data/aq_quarterly.csv")



