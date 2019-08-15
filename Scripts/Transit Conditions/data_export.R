#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl", "magrittr")
pack(packages)
rm(pack, packages)

source("data_processing.R")


#graph 1: average vehicle age
all_agency_average_class <- vehicleAge %>% filter(class != "Other") %>%
  group_by(year, class) %>%
  summarize(average_age = weighted.mean(avgAge, nVehicles),
            agency = "All Agencies")

single_agency_average_class <- vehicleAge %>% filter(class != "Other") %>%
  group_by(year, agency_name, class) %>%
  summarize(average_age = weighted.mean(avgAge, nVehicles)) %>%
  rename(agency = agency_name)

all_agency_average_all <- vehicleAge %>% filter(class != "Other") %>%
  group_by(year) %>%
  summarize(average_age = weighted.mean(avgAge, nVehicles),
            agency = "All Agencies",
            class = "All Vehicles")

single_agency_average_all <- vehicleAge %>% filter(class != "Other") %>%
  group_by(year, agency_name) %>%
  summarize(average_age = weighted.mean(avgAge, nVehicles),
            class = "All Vehicles") %>%
  rename(agency = agency_name)

graph_1 <- bind_rows(all_agency_average_class, single_agency_average_class, 
                         all_agency_average_all, single_agency_average_all) %>%
  mutate(agency_class = paste(agency, class, sep = "- ")) %>%
  select(-agency, -class) %>%
  spread(agency_class, average_age) %>%
  select(-'PART- All Vehicles', -'PATCO- All Vehicles') %>%
  round(2)

rm(all_agency_average_class, single_agency_average_class, 
   all_agency_average_all, single_agency_average_all)

#graph 2: rail vehicles stacked bar
all_agency_rail <- vehicleAge %>% 
  filter(class == "Rail Vehicles") %>%
  group_by(year) %>% 
  summarize(agency = "All Agencies", 
            age_0_5 = sum(age_0_5), age_6_11 = sum(age_6_11), 
            age_12_15 = sum(age_12_15), age_16_20 = sum(age_16_20), 
            age_21_25 = sum(age_21_25), age_25over = sum(age_25over))
  

single_agency_rail <- vehicleAge %>%
  filter(class == "Rail Vehicles")%>% 
  group_by(year, agency_name) %>% 
  summarize(age_0_5 = sum(age_0_5), age_6_11 = sum(age_6_11), 
            age_12_15 = sum(age_12_15), age_16_20 = sum(age_16_20), 
            age_21_25 = sum(age_21_25), age_25over = sum(age_25over))  %>%
  rename(agency = agency_name)

graph_2 <- bind_rows(all_agency_rail, single_agency_rail) %>%
  gather(age_bracket, n, 3:8) %>%
  mutate(agency_bracket = paste(agency, age_bracket, sep = "- ")) %>%
  select(-agency, -age_bracket) %>%
  arrange(year, agency_bracket) %>%
  select(year, agency_bracket, n) %>%
  spread(agency_bracket, n) %>%
  select(year,
         'All Agencies- age_0_5', 'All Agencies- age_6_11', 'All Agencies- age_12_15',
         'All Agencies- age_16_20', 'All Agencies- age_21_25', 'All Agencies- age_25over',
         'NJ Transit- age_0_5', 'NJ Transit- age_6_11', 'NJ Transit- age_12_15',
         'NJ Transit- age_16_20', 'NJ Transit- age_21_25', 'NJ Transit- age_25over',       
         'PATCO- age_0_5', 'PATCO- age_6_11', 'PATCO- age_12_15', 
         'PATCO- age_16_20', 'PATCO- age_21_25', 'PATCO- age_25over',
         'SEPTA- age_0_5', 'SEPTA- age_6_11', 'SEPTA- age_12_15',
         'SEPTA- age_16_20', 'SEPTA- age_21_25', 'SEPTA- age_25over')

rm(all_agency_rail, single_agency_rail)

#graph 3: buses stacked bar
all_agency_bus <- vehicleAge %>% 
  filter(class == "Buses") %>%
  group_by(year) %>% 
  summarize(agency = "All Agencies", 
            age_0_5 = sum(age_0_5), age_6_11 = sum(age_6_11), 
            age_12_15 = sum(age_12_15), age_16_20 = sum(age_16_20), 
            age_21_25 = sum(age_21_25), age_25over = sum(age_25over))


single_agency_bus <- vehicleAge %>%
  filter(class == "Buses")%>% 
  group_by(year, agency_name) %>% 
  summarize(age_0_5 = sum(age_0_5), age_6_11 = sum(age_6_11), age_12_15 = sum(age_12_15),
            age_16_20 = sum(age_16_20), age_21_25 = sum(age_21_25), age_25over = sum(age_25over))  %>%
  rename(agency = agency_name)

graph_3 <- bind_rows(all_agency_bus, single_agency_bus) %>%
  gather(age_bracket, n, 3:8) %>%
  mutate(agency_bracket = paste(agency, age_bracket, sep = "- ")) %>%
  select(-agency, -age_bracket) %>%
  arrange(year, agency_bracket) %>%
  select(year, agency_bracket, n) %>%
  spread(agency_bracket, n) %>%
select(year,
       'All Agencies- age_0_5', 'All Agencies- age_6_11', 'All Agencies- age_12_15',
       'All Agencies- age_16_20', 'All Agencies- age_21_25', 'All Agencies- age_25over',
       'NJ Transit- age_0_5', 'NJ Transit- age_6_11', 'NJ Transit- age_12_15',
       'NJ Transit- age_16_20', 'NJ Transit- age_21_25', 'NJ Transit- age_25over',       
       'PART- age_0_5', 'PART- age_6_11', 'PART- age_12_15', 
       'PART- age_16_20', 'PART- age_21_25', 'PART- age_25over',
       'SEPTA- age_0_5', 'SEPTA- age_6_11', 'SEPTA- age_12_15',
       'SEPTA- age_16_20', 'SEPTA- age_21_25', 'SEPTA- age_25over')

rm(all_agency_bus, single_agency_bus)

#export CSVs
write_csv(graph_1, "../Processed Data/transit conditions graph_1.csv")
write_csv(graph_2, "../Processed Data/transit conditions graph_2.csv")
write_csv(graph_3, "../Processed Data/transit conditions graph_3.csv")