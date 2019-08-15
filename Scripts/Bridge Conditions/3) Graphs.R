#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "magrittr", "sf", "tidycensus", "readxl", "units", "scales")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import data
source("2) Data Processing.R")

#define graph theme
graph_theme <- theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=8,face="bold"),
                     legend.title=element_text(size=10,face="bold"),
                     legend.text=element_text(size=8))

#Graph 1A total deficient bridge deck area by maintenance responsibility for a single geography
condition_deficient_main %>%
  group_by(year, maintainer_class) %>%
  summarize(area = sum(area)) %>%
  ggplot() +
  geom_col(aes(x = year, y = area, fill = fct_rev(maintainer_class))) +
  labs(title = "Structurally Deficient Bridge Deck Area by Maintainer,\nDVRPC Region, 2000-2018",
       fill = "Maintenance \nResponsibility",
       y = "Structurally Deficient Deck Area (millions of square feet)")+
  graph_theme

condition_deficient_main %>%
  ggplot() +
  geom_col(aes(x = year, y = area, fill = fct_rev(maintainer_class))) +
  labs(title = "Structurally Deficient Bridge Deck Area by Maintainer and State,\nDVRPC Region, 2000-2018",
       fill = "Maintenance \nResponsibility",
       y = "Structurally Deficient Deck Area (millions of square feet)") +
  facet_wrap(~state) +
  graph_theme

bridges_region %>%
  filter(condition == "Poor") %>%
  group_by(year, county, maintainer_class, condition) %>%
  summarize(count = n(),
            area = sum(area_sqft) / 1000000) %>%
  ggplot() +
  geom_col(aes(x = year, y = area, fill = fct_rev(maintainer_class))) +
  labs(title = "Structurally Deficient Bridge Deck Area by Maintainer and State,\nDVRPC Region, 2000-2018",
       fill = "Maintenance \nResponsibility",
       y = "Structurally Deficient Deck Area (millions of square feet)") +
  facet_wrap(~county) +
  graph_theme


#Graph 1B total deficient bridges by maintenance responsibility for a single geography
condition_deficient_main %>%
  group_by(year, maintainer_class) %>%
  summarize(count = sum(count)) %>%
  ggplot() +
  geom_col(aes(x = year, y = count, fill = maintainer_class)) +
  labs(title = "Structurally Deficient Bridge Deck Area by Maintainer,\nDVRPC Region, 2000-2018",
       fill = "Maintenance \nResponsibility",
       y = "number of structurally deficient bridges") +
  graph_theme

condition_deficient_main %>%
  ggplot() +
  geom_col(aes(x = year, y = count, fill = maintainer_class)) +
  labs(title = "Structurally Deficient Bridges by Maintainer,\nDVRPC Region, 2000-2018",
       fill = "Maintenance \n Responsibility",
       y = "number of structurally deficient bridges")+
  facet_wrap(~state) +
  graph_theme


#Graph total bridges (all conditions) by maintenance responsibility for a single geography
condition_count_main %>%
  group_by(year, maintainer_class, condition) %>%
  summarize(count = sum(count)) %>%
  ggplot() +
  geom_col(aes(x = year, y = count, fill = forcats::fct_rev(condition))) +
  labs(title = "Condition of Bridges in DVRPC Region by State and Maintainer, 2000-2018",
       fill = "Condition",
       y = "Bridges") +
  facet_grid(maintainer_class ~ .) + 
  graph_theme

condition_count_main %>%
  ggplot() +
  geom_col(aes(x = year, y = count, fill = forcats::fct_rev(condition))) +
  labs(title = "Condition of Bridges in DVRPC Region \nby State and Maintainer, 2000-2018",
       fill = "Condition",
       y = "Bridges") +
  facet_grid(maintainer_class ~ state) + 
  graph_theme

#Graph total bridge deck area (all conditions) by maintenance responsibility for a single geography
condition_count_main %>%
  group_by(year, maintainer_class, condition) %>%
  summarize(area = sum(area)) %>%
  ggplot() +
  geom_col(aes(x = year, y = area, fill = forcats::fct_rev(condition))) +
  labs(title = "Bridge Deck Area Condition in DVRPC Region \nby State and Maintainer, 2000-2018",
       fill = "Condition",
       y = "Structurally Deficient Deck Area (millions of square feet)") + 
  facet_grid(maintainer_class ~ .) +
  graph_theme

condition_count_main %>% 
  ggplot() +
  geom_col(aes(x = year, y = area, fill = forcats::fct_rev(condition))) +
  labs(title = "Bridge Deck Area Condition in DVRPC Region \nby State and Maintainer, 2000-2018",
       fill = "Condition",
       y = "Structurally Deficient Deck Area (millions of square feet)") + 
  facet_grid(maintainer_class ~ state) +
  graph_theme

bridges_region %>%
  group_by(year, county, maintainer_class, condition) %>%
  summarize(count = n(),
            area = sum(area_sqft) / 1000000) %>%
  ggplot() +
  geom_col(aes(x = year, y = area, fill = fct_rev(condition))) +
  labs(title = "Bridge Deck Area Condition in DVRPC Region \nby State and Maintainer, 2000-2018",
       fill = "Condition",
       y = "Structurally Deficient Deck Area (millions of square feet)") + 
  facet_grid(maintainer_class ~ county) +
  graph_theme