#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl", "sf", "units", "scales")
pack(packages)
rm(pack, packages)

#import new jersey county layer and variables
nj_county_sf <- st_read("../Data/NJ Counties/nj_counties.shp") %>%
  select(county = CO_NAME)
county_crs <- st_crs(nj_county_sf)
nj_county_names <- c("Burlington", "Camden", "Gloucester", "Mercer")


#import new jersey watershed data
nj_raw_2014 <- st_read("../Data/2014 NJ/2014_NJ_Integrated_List.shp") %>%
  st_transform(county_crs) %>%
  st_intersection(nj_county_sf) %>%
  filter(county %in% nj_county_names) %>%
  select(name = AU_NAME, attainment_aquatic_life = AQUATICLIF) %>%
  mutate(attainment_aquatic_life = as.character(attainment_aquatic_life),
         year = 2014)

nj_raw_2012 <- st_read("../Data/Old Data/Old NJ Data/2012/Envr_mon_water_IR_2012_use.shp") %>%
  st_transform(county_crs) %>%
  st_intersection(nj_county_sf) %>%
  filter(county %in% nj_county_names) %>%
  select(name = SW_NAME, attainment_aquatic_life = AQUATICLIF) %>%
  mutate(attainment_aquatic_life = as.character(attainment_aquatic_life),
         year = 2012)

nj_raw_2010 <- st_read("../Data/Old Data/Old NJ Data/2010/ir_2010_designateduse.shp") %>%
  st_transform(county_crs) %>%
  st_intersection(nj_county_sf) %>%
  filter(county %in% nj_county_names) %>%
  select(name = SW_NAME, attainment_aquatic_life = AqLife) %>%
  mutate(attainment_aquatic_life = as.character(attainment_aquatic_life),
         year = 2010)

nj_raw_2008 <- st_read("../Data/Old Data/Old NJ Data/2008/ir_2008_designateduse.shp") %>%
  st_transform(county_crs) %>%
  st_intersection(nj_county_sf) %>%
  filter(county %in% nj_county_names) %>%
  select(name = Name, attainment_aquatic_life = Aquatic) %>%
  mutate(attainment_aquatic_life = as.character(attainment_aquatic_life), 
         year = 2008)

nj_raw_2006 <- st_read("../Data/Old Data/Old NJ Data/2006/ir_2006.shp") %>%
  st_transform(county_crs) %>%
  st_intersection(nj_county_sf) %>%
  filter(county %in% nj_county_names) %>%
  select(name = SUBWATERSH, attainment_aquatic_life = AQL_GEN) %>%
  mutate(attainment_aquatic_life = as.character(attainment_aquatic_life),
         year = 2006)

#add areas
nj_raw_2014 %<>% mutate(acres = as.numeric(set_units(st_area(nj_raw_2014), "miles^2")) * 640)
nj_raw_2012 %<>% mutate(acres = as.numeric(set_units(st_area(nj_raw_2012), "miles^2")) * 640)
nj_raw_2010 %<>% mutate(acres = as.numeric(set_units(st_area(nj_raw_2010), "miles^2")) * 640)
nj_raw_2008 %<>% mutate(acres = as.numeric(set_units(st_area(nj_raw_2008), "miles^2")) * 640)
nj_raw_2006 %<>% mutate(acres = as.numeric(set_units(st_area(nj_raw_2006), "miles^2")) * 640)


#replace sublists with Supporting, Impaired or Insufficient Data
for(i in 1:length(nj_raw_2014$name)){
  nj_raw_2014$attainment_aquatic_life[i]<- 
    ifelse(nj_raw_2014$attainment_aquatic_life[i] %in% c("Sublist 1", "Sublist 2"), "Attaining",
         ifelse(nj_raw_2014$attainment_aquatic_life[i] == "Sublist 3", "Insufficient Data",
         "Impaired"))}

for(i in 1:length(nj_raw_2012$name)){
  nj_raw_2012$attainment_aquatic_life[i]<- 
    ifelse(nj_raw_2012$attainment_aquatic_life[i] %in% c("Sublist 1", "Sublist 2"), "Attaining",
           ifelse(nj_raw_2012$attainment_aquatic_life[i] == "Sublist 3", "Insufficient Data",
                  "Impaired"))}

for(i in 1:length(nj_raw_2010$name)){
  nj_raw_2010$attainment_aquatic_life[i]<- 
    ifelse(nj_raw_2010$attainment_aquatic_life[i] == "Fully Supporting", "Attaining",
           ifelse(nj_raw_2010$attainment_aquatic_life[i] == "Insufficient Information", "Insufficient Data",
                  "Impaired"))}

for(i in 1:length(nj_raw_2008$name)){
  nj_raw_2008$attainment_aquatic_life[i]<- 
    ifelse(nj_raw_2008$attainment_aquatic_life[i] %in% c("1", "2"), "Attaining",
           ifelse(nj_raw_2008$attainment_aquatic_life[i] == "3", "Insufficient Data",
                  "Impaired"))}

for(i in 1:length(nj_raw_2006$name)){
  nj_raw_2006$attainment_aquatic_life[i]<- 
    ifelse(nj_raw_2006$attainment_aquatic_life[i] %in% c("Sublist 1", "Sublist 2"), "Attaining",
           ifelse(nj_raw_2006$attainment_aquatic_life[i] == "Sublist 3", "Insufficient Data",
                  "Impaired"))}

#combine tables into single df
nj_all <- rbind(nj_raw_2014, nj_raw_2012, nj_raw_2010, nj_raw_2008, nj_raw_2006) %>%
  select(name, year, acres, attainment_aquatic_life)

nj_all$attainment_aquatic_life %<>% factor(levels = c("Attaining", "Insufficient Data", "Impaired"))


rm(nj_raw_2014, nj_raw_2012, nj_raw_2010, nj_raw_2008, nj_raw_2006, 
   i, county_crs, nj_county_names, nj_county_sf)

#create table summarizing nj data
nj_summary <- nj_all %>%
  as.data.frame() %>%
  group_by(year, attainment_aquatic_life) %>%
  summarize(acres = sum(acres)) %>%
  mutate(acres = acres / 1000,
         acres = round(acres, 1))

#recreate earlier tracking progess graph
ggplot(nj_summary, aes(x = year, y = acres, fill = attainment_aquatic_life)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = comma(acres)), size = 4, color = "white", fontface = "bold",
            position = position_fill(reverse = TRUE, vjust = 0.5)) +
  scale_fill_manual(values = c("darkgreen", "grey50", "darkorange")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(2006, 2014, 2), labels = seq(2006, 2014, 2)) +
  labs(fill = "Attainment Status", y = "Percent of Watershed Area")

#export table for web
nj_summary_spread <- nj_summary %>%
  spread(attainment_aquatic_life, acres)

write.csv(nj_summary_spread, "../Processed Data/Graph_2.csv", row.names=FALSE)
