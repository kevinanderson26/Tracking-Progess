#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "sf", "lwgeom")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import data
source("Scripts/Land Consumption/Land Preservation/1) data import.R")

#intersect preservation layers with planning areas layer
pa_intersect <- function(year, geo_layer){
  temp <- st_intersection(st_make_valid(year), geo_layer)
  temp$area_sqm <- st_area(temp) %>% as.numeric()
  temp$area_acres <- temp$area_sqm / 4046.86
  return(temp)}

pres_2004_pa <- pa_intersect(pres_2004, planning_areas)
pres_2007_pa <- pa_intersect(pres_2007, planning_areas)
pres_2011_pa <- pa_intersect(pres_2011, planning_areas)
pres_2016_pa <- pa_intersect(pres_2016, planning_areas)

pres_2004_co <- pa_intersect(pres_2004, counties)
pres_2007_co <- pa_intersect(pres_2007, counties)
pres_2011_co <- pa_intersect(pres_2011, counties)
pres_2016_co <- pa_intersect(pres_2016, counties)

pres_2004_sub <- pa_intersect(pres_2004, subregions)
pres_2007_sub <- pa_intersect(pres_2007, subregions)
pres_2011_sub <- pa_intersect(pres_2011, subregions)
pres_2016_sub <- pa_intersect(pres_2016, subregions)

#summarize acres of preserved land by type and planning area

pres_sum <- function(table, geo, yr) {
  geo <- enquo(geo)
  as.data.frame(table) %>% 
    group_by(!!geo, pres_type) %>%
    summarize(acres = sum(area_acres), year = yr, pres_class = first(pres_class)) %>%
    ungroup()}

pres_2004_pa_summary <- pres_sum(pres_2004_pa, planning_area, 2004)
pres_2007_pa_summary <- pres_sum(pres_2007_pa, planning_area, 2007)
pres_2011_pa_summary <- pres_sum(pres_2011_pa, planning_area, 2011)
pres_2016_pa_summary <- pres_sum(pres_2016_pa, planning_area, 2016)

pres_pa_sum <- bind_rows(pres_2004_pa_summary, pres_2007_pa_summary, 
                         pres_2011_pa_summary, pres_2016_pa_summary) %>%
  select(planning_area, year, pres_type, pres_class, everything())

rm(pres_2004_pa_summary, pres_2007_pa_summary, 
   pres_2011_pa_summary, pres_2016_pa_summary)

pres_2004_co_summary <- pres_sum(pres_2004_co, county, 2004)
pres_2007_co_summary <- pres_sum(pres_2007_co, county, 2007)
pres_2011_co_summary <- pres_sum(pres_2011_co, county, 2011)
pres_2016_co_summary <- pres_sum(pres_2016_co, county, 2016)

pres_co_sum <- bind_rows(pres_2004_co_summary, pres_2007_co_summary, 
                         pres_2011_co_summary, pres_2016_co_summary) %>%
  select(county, year, pres_type, pres_class, everything())

rm(pres_2004_co_summary, pres_2007_co_summary, 
   pres_2011_co_summary, pres_2016_co_summary)

pres_2004_sub_summary <- pres_sum(pres_2004_sub, subregion, 2004)
pres_2007_sub_summary <- pres_sum(pres_2007_sub, subregion, 2007)
pres_2011_sub_summary <- pres_sum(pres_2011_sub, subregion, 2011)
pres_2016_sub_summary <- pres_sum(pres_2016_sub, subregion, 2016)

pres_sub_sum <- bind_rows(pres_2004_sub_summary, pres_2007_sub_summary, 
                          pres_2011_sub_summary, pres_2016_sub_summary) %>%
  select(subregion, year, pres_type, pres_class, everything())

rm(pres_2004_sub_summary, pres_2007_sub_summary, 
   pres_2011_sub_summary, pres_2016_sub_summary)

#calcuate total areas of each geopgraphy, join to summary table, calculate preserved percentages
planning_areas$area_acres <- st_area(planning_areas) %>% as.numeric() / 4046.86

pa_areas <- planning_areas %>%
  as.data.frame() %>%
  select(planning_area, area_acres) %>%
  group_by(planning_area) %>%
  summarize(area_total = sum(area_acres))

pres_pa_sum %<>% left_join(pa_areas, by = "planning_area") %>%
  mutate(pres_percent = acres / area_total)

counties$area_acres <- st_area(counties) %>% as.numeric() / 4046.86 

county_areas <- counties %>%
  as.data.frame() %>%
  select(county, area_acres) %>%
  group_by(county) %>%
  summarize(area_total = sum(area_acres))

pres_co_sum %<>% left_join(county_areas, by = "county") %>%
  mutate(pres_percent = acres / area_total)

subregions$area_acres <- st_area(subregions) %>% as.numeric() / 4046.86 

subregion_areas <- subregions %>%
  as.data.frame() %>%
  select(subregion, area_acres) %>%
  group_by(subregion) %>%
  summarize(area_total = sum(area_acres))

pres_sub_sum %<>% left_join(subregion_areas, by = "subregion") %>%
  mutate(pres_percent = acres / area_total)


#revise county summary to move all municipal land in Philadelphia to county

for(i in 1:length(pres_co_sum$county)){
  if(pres_co_sum$county[i] == "Philadelphia" & pres_co_sum$pres_type[i] == "Municipal") {
    pres_co_sum$pres_type[i] <- "County"
    pres_co_sum$pres_class[i] <- "Public"}}

pres_co_sum %<>% group_by(county, year, pres_type) %>%
  summarize(pres_class = first(pres_class),
            acres = sum(acres),
            area_total = first(area_total),
            pres_percent = acres / area_total) %>%
  ungroup()


