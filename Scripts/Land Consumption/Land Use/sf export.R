#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl")
pack(packages)
rm(pack, packages)

#load raw data
source("Scripts/Land Consumption/Land Use/sf processing.R")

#graph 1A: line graphs of total developed acres for whole region, subregions, counties, and planning areas
scaling_factor_acres <- 1000
scaling_factor_per_cap <- 1000

rounding_factor_acres <- 1
rounding_factor_per_cap <- 2
  

graph_1_fun <- function(df, filter_group, filter_rate){
  filter_rate <- enquo(filter_rate)
  column_label <- case_when(filter_group == "Developed" & 
                              quo_name(filter_rate) == "area_acres" ~ "Total Developed Acres",
                            filter_group == "Developed" & 
                              quo_name(filter_rate) == "area_per_cap" ~ "Developed Acres per Capita",
                            filter_group == "Agricultural" & 
                              quo_name(filter_rate) == "area_acres" ~ "Total Agricultural Acres",
                            filter_group == "Agricultural" & 
                              quo_name(filter_rate) == "area_per_cap" ~ "Agricultural Acres per Capita")
  
  df %>%
    group_by(year, geography, lu_group) %>%
    summarize(pop = first(pop),
              area_acres = sum(area_acres) / scaling_factor_acres,
              area_per_cap = area_acres / pop * scaling_factor_per_cap) %>%
    ungroup() %>%
    mutate(area_acres = round(area_acres, rounding_factor_acres),
           area_per_cap = round(area_per_cap, rounding_factor_per_cap)) %>%
    filter(lu_group == filter_group) %>%
    mutate(geo_group = paste(column_label, geography, sep = "- ")) %>%
    select(year, geo_group, !!filter_rate) %>%
    spread(geo_group, !!filter_rate)
  }

graph_1A_region <- graph_1_fun(landuse_region, "Developed", area_acres)
graph_1A_subregion <- graph_1_fun(landuse_subregion, "Developed", area_acres)
graph_1A_planning_area <- graph_1_fun(landuse_planning_area, "Developed", area_acres)
graph_1A_county <- graph_1_fun(landuse_county, "Developed", area_acres)

graph_1A <- left_join(graph_1A_region, graph_1A_subregion, by = "year") %>%
  left_join(graph_1A_planning_area, by = "year") %>%
  left_join(graph_1A_county, by = "year")

rm(graph_1A_region, graph_1A_subregion, graph_1A_planning_area, graph_1A_county)

#graph 1B: line graphs of developed acres per capita for whole region, subregions, counties, and planning areas
graph_1B_region <- graph_1_fun(landuse_region, "Developed", area_per_cap)
graph_1B_subregion <- graph_1_fun(landuse_subregion, "Developed", area_per_cap)
graph_1B_planning_area <- graph_1_fun(landuse_planning_area, "Developed", area_per_cap)
graph_1B_county <- graph_1_fun(landuse_county, "Developed", area_per_cap)

graph_1B <- left_join(graph_1B_region, graph_1B_subregion, by = "year") %>%
  left_join(graph_1B_planning_area, by = "year") %>%
  left_join(graph_1B_county, by = "year")

rm(graph_1B_region, graph_1B_subregion, graph_1B_planning_area, graph_1B_county)

#graph 1C: line graphs of total agricultural acres for whole region, subregions, counties, and planning areas
graph_1C_region <- graph_1_fun(landuse_region, "Agricultural", area_acres)
graph_1C_subregion <- graph_1_fun(landuse_subregion, "Agricultural", area_acres)
graph_1C_planning_area <- graph_1_fun(landuse_planning_area, "Agricultural", area_acres)
graph_1C_county <- graph_1_fun(landuse_county, "Agricultural", area_acres)

graph_1C <- left_join(graph_1C_region, graph_1C_subregion, by = "year") %>%
  left_join(graph_1C_planning_area, by = "year") %>%
  left_join(graph_1C_county, by = "year")

rm(graph_1C_region, graph_1C_subregion, graph_1C_planning_area, graph_1C_county)

#graph 1D: line graphs of agricultural acres per capita for whole region, subregions, counties, and planning areas
graph_1D_region <- graph_1_fun(landuse_region, "Agricultural", area_per_cap)
graph_1D_subregion <- graph_1_fun(landuse_subregion, "Agricultural", area_per_cap)
graph_1D_planning_area <- graph_1_fun(landuse_planning_area, "Agricultural", area_per_cap)
graph_1D_county <- graph_1_fun(landuse_county, "Agricultural", area_per_cap)

graph_1D <- left_join(graph_1D_region, graph_1D_subregion, by = "year") %>%
  left_join(graph_1D_planning_area, by = "year") %>%
  left_join(graph_1D_county, by = "year")

rm(graph_1D_region, graph_1D_subregion, graph_1D_planning_area, graph_1D_county)

graph_1 <- left_join(graph_1A, graph_1B, by = "year") %>%
  left_join(graph_1C, by = "year") %>%
  left_join(graph_1D, by = "year")

rm(graph_1A, graph_1B, graph_1C, graph_1D)

#graph 2: line graphs of total developed acres for whole region, subregions, counties, and planning areas

graph_2_fun <- function(df){df %>%
    mutate(area_acres = area_acres / scaling_factor_acres,
           area_acres = round(area_acres, rounding_factor_acres),
           lu_geo = paste(geography, lu_name_adj, sep = "- ")) %>%
    select(year, lu_geo, area_acres) %>%
    spread(lu_geo, area_acres)
    }

graph_2_region <- graph_2_fun(landuse_region)
graph_2_subregion <- graph_2_fun(landuse_subregion)
graph_2_pa <- graph_2_fun(landuse_planning_area)
graph_2_county <- graph_2_fun(landuse_county)


graph_2 <- left_join(graph_2_region, graph_2_subregion, by = "year") %>%
  left_join(graph_2_pa, by = "year") %>%
  left_join(graph_2_county, by = "year")

rm(graph_2_region, graph_2_subregion, graph_2_pa, graph_2_county)

#write csvs
write_csv(graph_1, "Outputs/Land Consumption/land consumption graph_1.csv")
write_csv(graph_2, "Outputs/Land Consumption/land consumption graph_2.csv")