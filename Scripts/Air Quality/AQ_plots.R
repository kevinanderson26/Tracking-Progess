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

#source input files
source("AQ_daily_input.R")
source("AQ_yearly_input.R")
source("AQ_processing_and_output.R")

#graph days per year for 2008-2017 with overlay 5-year moving average
aqiYearlyFiltered %>% filter(year > 1980) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = daysViolating), fill = "blue4", width = .50) +
  geom_line(aes(y = fiveYearAvg), col = "chartreuse3", lwd = 1.25) +
  scale_x_continuous(breaks = seq(from = 1981, to = 2016, by = 5)) +
  scale_y_continuous(limits = c(0,365)) +
  ylab("Number of Days") +
  ggtitle("Days Air Quality Exceeded the NAAQS, 1980-2017") +
  theme(title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

#scatterplot showing AQI daily values
ggplot(aqiDaily, aes(x = date)) +
  geom_point(aes(y = aqiValueOzone, fill = aqiCategoryOzone, size = aqiValueOzone), color = "black", shape = 21) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_size_continuous(range = c(1, 2.5))

ggplot(aqiDaily, aes(x = date)) +
  geom_point(aes(y = aqiValuePM, fill = aqiCategoryPM, size = aqiValuePM), color = "black", shape = 21) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_size_continuous(range = c(1, 2.5))

#line graph showing average AQI for ozone and pmi by month and year
aqiDaily %>% group_by(monthYear) %>% 
  summarize(ozone = mean(aqiValueOzone), pm = mean(aqiValuePM)) %>% 
  gather("pollutant", "AQI", 2:3) %>%
  ggplot(aes(x = month, group = pollutant)) +
  geom_line(aes(y = AQI, col = pollutant))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))

#Stacked bar chart showing aqi categories by month and year, ozone.
aqiDaily %>% group_by(monthYear, aqiCategoryOzone) %>% 
  summarize(n_level = n()) %>%
  ggplot() +
  geom_col(aes(x = month, y = n_level, fill = aqiCategoryOzone, col = aqiCategoryOzone), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#Stacked bar chart showing aqi categories by month and year, pm.
aqiDaily %>% group_by(monthYear, aqiCategoryPM) %>% 
  summarize(nLevel = n()) %>%
  ggplot() +
  geom_col(aes(x = month, y = nLevel, fill = aqiCategoryPM, col = aqiCategoryPM), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#Stacked bar chart showing aqi categories by year, ozone.
aqiDaily %>% group_by(year, aqiCategoryOzone) %>% 
  summarize(n_level = n()) %>%
  ggplot() +
  geom_col(aes(x = year, y = n_level, fill = aqiCategoryOzone), col = "black", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#Stacked bar chart showing aqi categories by year, pm.
aqiDaily %>% group_by(year, aqiCategoryPM) %>% 
  summarize(n_level = n()) %>%
  ggplot() +
  geom_col(aes(x = year, y = n_level, fill = aqiCategoryPM), col = "black", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

aqiDaily %>% group_by(quarterYear, aqiCategoryOzone) %>% 
  summarize(nLevel = n()) %>%
  ggplot() +
  geom_col(aes(x = quarterYear, y = nLevel, fill = aqiCategoryOzone), col = "black", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#quarterly summary, only unhealthy categories
aqiDailyFiltered %>% gather("aqiCategory", "n", 2:7) %>%
  filter(aqiCategory %in% c("unhealthySensitiveOzone", "unhealthyOzone", "veryUnhealthyOzone")) %>%
  ggplot() +
  geom_col(aes(x = quarterYear, y = n, fill = aqiCategory, col = aqiCategory), position = position_stack(reverse = TRUE)) +
  scale_colour_manual(values = c("orange", "red", "brown")) +
  scale_fill_manual(values = c("orange", "red", "brown")) +
  ylab("Number of Days") +
  scale_y_continuous(limits = c(0,91)) +
  ggtitle("Ozone AQI Categories per quarter, 1980-2017") +
  theme(title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))

aqiDailyFiltered %>% gather("aqiCategory", "n", 2:7) %>%
  filter(aqiCategory %in% c("unhealthySensitivePM", "unhealthyPM", "veryUnhealthyPM")) %>%
  ggplot() +
  geom_col(aes(x = quarterYear, y = n, fill = aqiCategory, col = aqiCategory), position = position_stack(reverse = TRUE)) +
  scale_colour_manual(values = c("orange", "red", "brown")) +
  scale_fill_manual(values = c("orange", "red", "brown")) +
  scale_y_continuous(limits = c(0,91)) +
  ylab("Number of Days") +
  ggtitle("PM2.5 AQI Categories per quarter, 1999-2017") +
  theme(title = element_text(size = 10), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8))