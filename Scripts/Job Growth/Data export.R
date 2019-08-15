#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl", "viridis")
pack(packages)
rm(pack, packages)



#import processed data
source("data processing.R")

#Graph 1A: Annual growth rate

regional_1A <- qcew_regional %>% select(year, DVRPC = annual_change)

subregional_1A <- qcew_subregional %>% select(year, subregion, annual_change) %>%
  spread(subregion, annual_change)
    
county_1A <- qcew_county %>% select(year, county, annual_change) %>%
  spread(county, annual_change)

graph_1A <- left_join(regional_1A, subregional_1A, by = "year") %>%
  left_join(county_1A, by = "year") %>%
  round(4)

rm(regional_1A, subregional_1A, county_1A)

#Graph 1B: Total Growth Rate

regional_1B <- qcew_regional %>% select(year, DVRPC = total_change)

subregional_1B <- qcew_subregional %>% select(year, subregion, total_change) %>%
  spread(subregion, total_change)

county_1B <- qcew_county %>% select(year, county, total_change) %>%
  spread(county, total_change)

graph_1B <- left_join(regional_1B, subregional_1B, by = "year") %>%
  left_join(county_1B, by = "year") %>%
  round(4)

rm(regional_1B, subregional_1B, county_1B)

#Graph 2: County Distribution
graph_2 <- county_percent %>% select(year, county, percent) %>%
  mutate(percent = round(percent, 3)) %>%
  spread(county, percent) 

#Graph 3: Industry Distribution
regional_3 <- cbp_regional %>% mutate(geo_sector = paste("DVRPC- ", sector, sep = "")) %>%
  select(year, geo_sector, emp_percent) %>%
  mutate(emp_percent = round(emp_percent, 3)) %>%
  spread(geo_sector, emp_percent)
  
subregional_3 <- cbp_subregional %>% mutate(geo_sector = paste(subregion, "- ", sector, sep = "")) %>%
  select(year, geo_sector, emp_percent) %>%
  mutate(emp_percent = round(emp_percent, 3)) %>%
  spread(geo_sector, emp_percent)

county_3 <- cbp_county %>% mutate(geo_sector = paste(county_name, "- ", sector, sep = "")) %>%
  select(year, geo_sector, emp_percent) %>%
  mutate(emp_percent = round(emp_percent, 3)) %>%
  spread(geo_sector, emp_percent)

graph_3 <- left_join(regional_3, subregional_3, by = "year") %>%
  left_join(county_3, by = "year")

rm(regional_3, subregional_3, county_3)

#export csv's

write_csv(graph_1A, "../Processed Data/jobs_graph1A.csv")
write_csv(graph_1B, "../Processed Data/jobs_graph1B.csv")
write_csv(graph_2, "../Processed Data/jobs_graph2.csv")
write_csv(graph_3, "../Processed Data/jobs_graph3.csv")
