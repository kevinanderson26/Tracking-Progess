munic_key <- st_read("../Data/Boundary Shapefiles/munic/munic.shp", stringsAsFactors = FALSE) %>% 
              select(geoid = GEOID)

highSchoolRatioMunicMap2016 <- highSchoolRatioMunic %>% filter(year == 2016) %>%
                                    left_join(munic_key, by = "geoid")

ggplot(highSchoolRatioMunicMap2016) +
  geom_sf(aes(fill = postHighSchool)) +
  scale_fill_viridis_c(option = "magma", direction = -1)

