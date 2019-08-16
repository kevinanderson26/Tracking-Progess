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

#import processed data
source("Scripts/Housing Activity/data_processing.R")

#Graph 1
permits_graph_1 <- units_ratio %>% select(year, ratio_annual, ratio_cumulative) %>%
  round(digits = 2)

permits_graph_1 %>%
  ggplot(aes(x = year)) +
    geom_line(aes(y = ratio_annual), color = "red") +
    geom_line(aes(y = ratio_cumulative), color = "black") +
    scale_y_continuous(limits = c(0, 100))

#Graph 2
permits_graph_2A <- units_PA %>%
  ungroup() %>%
  mutate(co_pa = paste("Annual- ", co_pa, sep = "")) %>%
  select(year, co_pa, units) %>%
  spread(co_pa, units)

permits_graph_2B <- units_PA %>%
  ungroup() %>%
  mutate(co_pa = paste("Cumulative- ", co_pa, sep = "")) %>%
  select(year, co_pa, csum_units) %>%
  spread(co_pa, csum_units)

units_PA %>% filter(county == "Montgomery County") %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = units, fill = planning_area))

#Graph 3
permits_graph_3A <- unitsType %>%
  ungroup() %>%
  mutate(geoType = paste("Annual- ", geoType, sep = "")) %>%
  select(year, geoType, units) %>%
  spread(geoType, units) %>%
  select(year, 
         'Annual- DVRPC- large multi-family', 'Annual- DVRPC- single family', 'Annual- DVRPC- small multi-family',
         'Annual- Core City- large multi-family', 'Annual- Core City- single family', 
         'Annual- Core City- small multi-family',  'Annual- Developed Community- large multi-family',
         'Annual- Developed Community- single family', 'Annual- Developed Community- small multi-family', 
         'Annual- Growing Suburb- large multi-family', 'Annual- Growing Suburb- single family', 
         'Annual- Growing Suburb- small multi-family', 'Annual- Rural Area- large multi-family', 
         'Annual- Rural Area- single family', 'Annual- Rural Area- small multi-family', everything())

permits_graph_3B <- unitsType %>%
  ungroup() %>%
  mutate(geoType = paste("Cumulative- ", geoType, sep = "")) %>%
  select(year, geoType, csum_units) %>%
  spread(geoType, csum_units) %>%
  select(year, 'Cumulative- DVRPC- large multi-family', 
         'Cumulative- DVRPC- single family', 'Cumulative- DVRPC- small multi-family',
         'Cumulative- Core City- large multi-family', 'Cumulative- Core City- single family', 
         'Cumulative- Core City- small multi-family',  'Cumulative- Developed Community- large multi-family',
         'Cumulative- Developed Community- single family', 'Cumulative- Developed Community- small multi-family', 
         'Cumulative- Growing Suburb- large multi-family', 'Cumulative- Growing Suburb- single family', 
         'Cumulative- Growing Suburb- small multi-family', 'Cumulative- Rural Area- large multi-family', 
         'Cumulative- Rural Area- single family', 'Cumulative- Rural Area- small multi-family', everything())

#save CSVs
write_csv(permits_graph_1, "Outputs/Housing Activity/permits_graph_1.csv")
write_csv(permits_graph_2A, "Outputs/Housing Activity/permits_graph_2A.csv")
write_csv(permits_graph_2B, "Outputs/Housing Activity/permits_graph_2B.csv")
write_csv(permits_graph_3A, "Outputs/Housing Activity/permits_graph_3A.csv")
write_csv(permits_graph_3B, "Outputs/Housing Activity/permits_graph_3B.csv")
