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

#cumulative units, by type
units %>% group_by(year, type) %>% 
  summarize(units = sum(units)) %>% 
  ungroup() %>%
  group_by(type) %>%
  mutate(csum = cumsum(units)) %>% 
ggplot(aes(x = year, y = csum, fill = type)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#cumulative units, by planning area
units %>% group_by(year, planning_area) %>% 
  summarize(units = sum(units)) %>% 
  ungroup() %>%
  group_by(planning_area) %>%
  mutate(csum = cumsum(units)) %>%
ggplot(aes(x = year, y = csum, fill = planning_area)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#cumulative units, by county
units %>% group_by(year, county_code) %>% 
  summarize(units = sum(units)) %>% 
  ungroup() %>%
  group_by(county_code) %>%
  mutate(csum = cumsum(units)) %>%
  ggplot(aes(x = year, y = csum, fill = county_code)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#cumulative units, by subregion
units %>% group_by(year, subregion) %>% 
  summarize(units = sum(units)) %>% 
  ungroup() %>%
  group_by(subregion) %>%
  mutate(csum = cumsum(units)) %>%
  ggplot(aes(x = year, y = csum, fill = subregion)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#cumulative units, by planning area and type
units %>% group_by(year, type_PA) %>% 
  summarize(units = sum(units), planning_area = first(planning_area)) %>% 
  ungroup() %>%
  group_by(type_PA) %>%
  mutate(csum = cumsum(units)) %>%
  ggplot(aes(x = year, y = csum, fill = type_PA)) +
  geom_area(position = "stack", lwd = 1) +
  scale_fill_manual(values = c("lightpink3", "red1", "red3",
                               "steelblue2", "royalblue1", "blue3",
                               "palegreen3", "green2", "darkgreen",
                               "gold2", "orange2", "orange3")) +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#cumulative units by type, Philadelphia
units %>% filter(county_code == "101") %>%
  group_by(year, type) %>% 
  summarize(units = sum(units)) %>% 
  ungroup() %>%
  group_by(type) %>%
  mutate(csum = cumsum(units)) %>%
  ggplot(aes(x = year, y = csum, fill = type)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  