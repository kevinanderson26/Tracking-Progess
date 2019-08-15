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

#import raw data
source("data_processing.R")

#bar graph of number of units per year
units %>% group_by(year) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar graph of number of units per year, by unit type
units %>% group_by(year, type) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(aes(fill = type), position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar graph of number of units per year, by planning area
units %>% group_by(year, planning_area) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(aes(fill = planning_area), position = "fill") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

units %>% group_by(year, planning_area) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(aes(fill = planning_area), position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar graph of number of units per year, by county
units %>% group_by(year, county_code) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(aes(fill = county_code), position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar graph of number of units per year, by subregion
units %>% group_by(year, subregion) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(aes(fill = subregion), position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar graph of number of units per year, by unit type and planning area
units %>% group_by(year, type_PA) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = year, y = units)) +
  geom_col(aes(fill = type_PA), position = "stack") +
  scale_fill_manual(values = c("lightpink3", "red1", "red3",
                                "steelblue2", "royalblue1", "blue3",
                                "palegreen3", "green2", "darkgreen",
                                "gold2", "orange2", "orange3")) +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#scaled bar graph of number of units per planning area, by unit type (single year)
units %>% filter(year == 2017) %>% 
  group_by(planning_area, type) %>% 
  summarize(units = sum(units)) %>%
  ggplot(aes(x = planning_area, y = units)) +
  geom_col(aes(fill = type), position = "fill")

