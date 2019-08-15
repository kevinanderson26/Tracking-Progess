#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl", "sf", "lwgeom")
pack(packages)
rm(pack, packages)

#import data
source("sf import.R")

#intersect land use and planning area layer (This is VERY SLOW. Took 23 minutes on the intern computer)
landuse_pa <- st_intersection(st_make_valid(landuse), planning_areas)
landuse_pa$area_sqm <- st_area(landuse_pa) %>% as.numeric()
landuse_pa$area_acres <- landuse_pa$area_sqm / 4046.86

#convert land use sf to data frame for faster calculations
landuse_pa_df <- landuse_pa %>% as_tibble() %>% select(-geometry)

#collapse all agriculture categories and manufacturing categories
landuse_pa_df %<>%
  mutate(lu_code = case_when(lu_code == "10010" ~ "10000",
                             lu_code == "03010" ~ "03000",
                             lu_code == "03019" ~ "03009",
                             TRUE ~ lu_code),
         lu_name = case_when(lu_code == "10000" ~ "Agriculture",
                             lu_code == "03000" ~ "Manufacturing",
                             lu_code == "03009" ~ "Parking: Manufacturing",
                             TRUE ~ lu_name))

#collapse all multi-family housing categories
landuse_pa_df %<>%
  mutate(lu_code = case_when(lu_code %in% c("02010", "02020") ~ "02000",
                             lu_code %in% c("02019", "02029") ~ "02009",
                             TRUE ~ lu_code),
         lu_name = case_when(lu_code == "02000" ~ "Residential: Multi-Family/Single-Family Attached",
                             lu_code == "02009" ~ "Parking: Multi-Family/Single-Family Attached",
                             TRUE ~ lu_name))

#correct community services parking miscoded as commercial parking
landuse_pa_df %<>% mutate(lu_name = case_when(lu_code == "07009" ~ "Parking: Community Services", 
                                              TRUE ~ lu_name))

#combine parking categories in adjusted name field
landuse_pa_df %<>% mutate(lu_name_adj = case_when(str_detect(lu_name, "^Parking") ~ "Parking", 
                                                  TRUE ~ lu_name))

#add agricultural/developed groups
ag_codes <- c("10000", "10009")
other_codes <- c("12000", "12010", "13000")

landuse_pa_df %<>% mutate(lu_group = case_when(lu_code %in% ag_codes ~ "Agricultural",
                                               lu_code %in% other_codes ~ "Water/Wooded/Vacant",
                                               TRUE ~ "Developed"))

rm(ag_codes, other_codes)

#add subregion field 
pa_suburbs <- c("Bucks", "Chester", "Delaware", "Montgomery")
nj_suburbs <- c("Burlington", "Camden", "Gloucester", "Mercer")

landuse_pa_df %<>% mutate(subregion = case_when(county %in% pa_suburbs ~ "Pennsylvania Suburbs",
                                                county %in% nj_suburbs ~ "New Jersey Suburbs",
                                                TRUE ~ "Philadelphia"))

rm(pa_suburbs, nj_suburbs)


#create sub-summaries for each geography and join population totals
landuse_region <- landuse_pa_df %>% 
    group_by(year, lu_name_adj) %>% 
    summarize(area_acres = sum(area_acres),
              lu_group = first(lu_group)) %>%
    ungroup() %>%
    mutate(geography = "DVRPC") %>%
    select(year, geography, lu_name_adj, lu_group, area_acres) %>%
    left_join(pop_region, by = "year")
  
landuse_subregion <- landuse_pa_df %>% 
    group_by(year, subregion, lu_name_adj) %>% 
    summarize(area_acres = sum(area_acres),
              lu_group = first(lu_group)) %>%
    ungroup() %>%
    select(year, subregion, lu_name_adj, lu_group, area_acres) %>%
    left_join(pop_subregion, by = c("year", "subregion")) %>%
    rename(geography = subregion) %>%
    filter(geography != "Philadelphia")
  
landuse_planning_area <- landuse_pa_df %>% 
    group_by(year, planning_area, lu_name_adj) %>% 
    summarize(area_acres = sum(area_acres),
              lu_group = first(lu_group)) %>%
    ungroup() %>%
    select(year, planning_area, lu_name_adj, lu_group, area_acres) %>%
    left_join(pop_pa, by = c("year", "planning_area")) %>%
    rename(geography = planning_area)

landuse_county <- landuse_pa_df %>% 
    group_by(year, county, lu_name_adj) %>% 
    summarize(area_acres = sum(area_acres),
              lu_group = first(lu_group)) %>%
    ungroup() %>%
    select(year, county, lu_name_adj, lu_group, area_acres) %>%
    left_join(pop_county, by = c("year", "county")) %>%
    rename(geography = county)

#graph of all land uses
landuse_pa_df %>%
  group_by(year, county, lu_name_adj) %>%
  summarize(area = sum(area_acres)) %>%
  ggplot() +
  geom_col(aes(x = year, y = area, fill = lu_name_adj), position = "stack") +
  facet_wrap(~county)