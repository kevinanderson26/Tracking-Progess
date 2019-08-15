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

# Graph 1: Percent of Road Segments in Deficient Condition
# 1) group by year, state and pavement quality and summarize to get total miles in each category for all road types
# 2) group by year and state and mutate to get percent of miles in each quality category
# 3) bind to original summary file
# 4) filter to just poor quality roads
# 5) create combined state and road type label; round, spread and organize variables

graph_1 <- summary_pavement %>% 
  group_by(year, state, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  group_by(year, state) %>%
  mutate(percent = miles / sum(miles),
         road_type_adjusted = "All") %>%
  ungroup() %>%
  bind_rows(summary_pavement) %>%
  filter(pavement_quality == "Poor") %>%
  mutate(state_type = paste(state, road_type_adjusted, sep = "- ")) %>%
  select(year, state_type, percent) %>%
  mutate(percent = round(percent, 3)) %>%
  spread(state_type, percent) %>%
  select('year',
         'DVRPC- All',
         'New Jersey- All',
         'Pennsylvania- All',
         'DVRPC- NHS, interstate',
         'New Jersey- NHS, interstate',
         'Pennsylvania- NHS, interstate',
         'DVRPC- NHS, non-interstate',
         'New Jersey- NHS, non-interstate',
         'Pennsylvania- NHS, non-interstate',
         'DVRPC- Non-NHS',
         'New Jersey- Non-NHS',
         'Pennsylvania- Non-NHS')

plot_1 <- summary_pavement %>% 
  group_by(year, state, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  group_by(year, state) %>%
  mutate(percent = miles / sum(miles),
         road_type_adjusted = "All") %>%
  ungroup() %>%
  bind_rows(summary_pavement) %>%
  filter(pavement_quality == "Poor") %>%
  mutate(percent = round(percent, 3)) %>%
ggplot() +
  geom_line(aes(x = year, y = percent, color = road_type_adjusted), lwd = 1.5) +
  geom_point(aes(x = year, y = percent, color = road_type_adjusted), 
             fill = "white", size = 4, stroke = 1.5, shape=21) +
  facet_grid(state~road_type_adjusted) + 
  theme_linedraw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                     minor_breaks = NULL)

ggsave("deficient_pavement_graph_1.pdf", plot_1, width = 18, height = 12)

#Graph 2: All Road segment conditions, grouped by road type and state
# 1) group by year, state and pavement quality and summarize to get total miles in each category for all road types
# 2) group by year and state and mutate to get percent of miles in each quality category
# 3) bind to original summary file
# 4) create combined state and road type label; spread

graph_2 <- summary_pavement %>% 
  group_by(year, state, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  ungroup() %>%
  mutate(road_type_adjusted = "All") %>%
  bind_rows(summary_pavement) %>%
  mutate(state_type = paste(state, ", ", road_type_adjusted, "- ", pavement_quality, sep = "")) %>%
  select(year, state_type, miles) %>%
  spread(state_type, miles)

plot_2 <- summary_pavement %>% 
  group_by(year, state, pavement_quality) %>%
  summarize(miles = sum(miles)) %>%
  ungroup() %>%
  mutate(road_type_adjusted = "All") %>%
  bind_rows(summary_pavement) %>%
  ggplot() +
    geom_col(aes(x = year, y = miles, fill = pavement_quality)) +
    facet_grid(state~road_type_adjusted) + 
    theme_linedraw() 

ggsave("deficient_pavement_graph_2.pdf", plot_2, width = 18, height = 12)

#write CSVs for web
write_csv(graph_1, "../Processed Data/pavement_graph_1.csv")
write_csv(graph_2, "../Processed Data/pavement_graph_2.csv")