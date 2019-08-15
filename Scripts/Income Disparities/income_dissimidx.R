library(tidycensus); library(tidyverse)

# Study area FIPS codes
dvrpc <- c("34005", "34007", "34015", "34021",
           "42017", "42029", "42045", "42091", "42101")

#API Key (tied to KAnderson@dvrpc.org, may need to be changed in the future)
census_api_key("9999d8f1945ed47effb2dabd2dcad9af8213909f", overwrite = TRUE, install = TRUE)

# API Calls for all 2010 and 2017 variables

# Check if 2017 data exists
# If not, download from API
if(!file.exists("./data/api_2017.csv")){
  # https://api.census.gov/data/2017/acs/acs5/variables.html
  collect <- get_acs(geography = "tract",
                     year = 2017,
                     state = c(34,42),
                     output = "wide",
                     variables = c(
                       racUniverse = "B02001_001E",
                       racWhite = "B02001_002E",
                       hispUniverse = "B03002_001E",
                       notHisp = "B03002_002E",
                       whiteNotHisp = "B03002_003E",
                       povUniverse = "B17024_001E",
                       povA.2 = "B17024_011E", povA.3 = "B17024_012E", 
                       povA.4 = "B17024_013E", povA.5 = "B17024_014E",
                       povB.2 = "B17024_024E", povB.3 = "B17024_025E",
                       povB.4 = "B17024_026E", povB.5 = "B17024_027E",
                       povC.2 = "B17024_037E", povC.3 = "B17024_038E",
                       povC.4 = "B17024_039E", povC.5 = "B17024_040E",
                       povD.2 = "B17024_050E", povD.3 = "B17024_051E",
                       povD.4 = "B17024_052E", povD.5 = "B17024_053E",
                       povE.2 = "B17024_063E", povE.3 = "B17024_064E",
                       povE.4 = "B17024_065E", povE.5 = "B17024_066E",
                       povF.2 = "B17024_076E", povF.3 = "B17024_077E",
                       povF.4 = "B17024_078E", povF.5 = "B17024_079E",
                       povG.2 = "B17024_089E", povG.3 = "B17024_090E",
                       povG.4 = "B17024_091E", povG.5 = "B17024_092E",
                       povH.2 = "B17024_102E", povH.3 = "B17024_103E",
                       povH.4 = "B17024_104E", povH.5 = "B17024_105E",
                       povI.2 = "B17024_115E", povI.3 = "B17024_116E",
                       povI.4 = "B17024_117E", povI.5 = "B17024_118E",
                       povJ.2 = "B17024_128E", povJ.3 = "B17024_129E",
                       povJ.4 = "B17024_130E", povJ.5 = "B17024_131E"
                     ))
  
  # Clean up fields
  # I compute above 200% of the Federal Poverty Level (FPL) here
  # So I can get rid of these extra columns ASAP
  collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
  collect <- collect %>%
    mutate(stcty = str_sub(GEOID, 1, 5)) %>%
    filter(stcty %in% dvrpc) %>%
    mutate(above200 = rowSums(.[,9:48]))
  collect[c(9:48)] <- NULL
  write_csv(collect, "api_2017.csv")
} else {
  collect <- read_csv("./data/api_2017.csv")
}

# Check if 2010 data exists
# If not, download from API
if(!file.exists("./data/api_2010.csv")){
  # API calls don't work anymore unless you call a single state
  api_bystate <- data.frame()
  for (st in c(34, 42)) {
    api <- get_acs(geography = "tract",
                   state = st,
                   year = 2010,
                   output = "wide",
                   variables = c(
                     racUniverse = "B02001_001E",
                     racWhite = "B02001_002E",
                     hispUniverse = "B03002_001E",
                     notHisp = "B03002_002E",
                     whiteNotHisp = "B03002_003E",
                     povUniverse = "B17024_001E",
                     povA.2 = "B17024_011E",
                     povA.3 = "B17024_012E",
                     povA.4 = "B17024_013E",
                     povA.5 = "B17024_014E",
                     povB.2 = "B17024_024E",
                     povB.3 = "B17024_025E",
                     povB.4 = "B17024_026E",
                     povB.5 = "B17024_027E",
                     povC.2 = "B17024_037E",
                     povC.3 = "B17024_038E",
                     povC.4 = "B17024_039E",
                     povC.5 = "B17024_040E",
                     povD.2 = "B17024_050E",
                     povD.3 = "B17024_051E",
                     povD.4 = "B17024_052E",
                     povD.5 = "B17024_053E",
                     povE.2 = "B17024_063E",
                     povE.3 = "B17024_064E",
                     povE.4 = "B17024_065E",
                     povE.5 = "B17024_066E",
                     povF.2 = "B17024_076E",
                     povF.3 = "B17024_077E",
                     povF.4 = "B17024_078E",
                     povF.5 = "B17024_079E",
                     povG.2 = "B17024_089E",
                     povG.3 = "B17024_090E",
                     povG.4 = "B17024_091E",
                     povG.5 = "B17024_092E",
                     povH.2 = "B17024_102E",
                     povH.3 = "B17024_103E",
                     povH.4 = "B17024_104E",
                     povH.5 = "B17024_105E",
                     povI.2 = "B17024_115E",
                     povI.3 = "B17024_116E",
                     povI.4 = "B17024_117E",
                     povI.5 = "B17024_118E",
                     povJ.2 = "B17024_128E",
                     povJ.3 = "B17024_129E",
                     povJ.4 = "B17024_130E",
                     povJ.5 = "B17024_131E"
                   ))
    api_bystate <- rbind(api_bystate, api)
    
    # Clean up fields
    collect_2 <- api_bystate[, -( grep("\\M$" , colnames(api_bystate), perl = TRUE))]
    collect_2 <- collect_2 %>%
      mutate(stcty = str_sub(GEOID, 1, 5)) %>%
      filter(stcty %in% dvrpc) %>%
      mutate(above200 = rowSums(.[,9:48])) # Above 200% FPL
    collect_2[c(9:48)] <- NULL
    write_csv(collect_2, "api_2010.csv")
  }
} else {
  collect_2 <- read_csv("./data/api_2010.csv")
}

# The rest of the data (1980, 1990, 2000) is from NHGIS
# When the file has _geog2010_ in it,
# Data has been harmonized to 2010 census tract boundaries
# When it is _nominal_ then the time series data is
# 'nominally integrated'

# Income
pov <- read_csv("./data/nhgis0003_ts_nominal_tract.csv") %>%
  mutate_at(vars(COUNTYFP), funs(str_pad(., width = 3, side = "left", pad = "0"))) %>%
  filter(paste0(STATEFP, COUNTYFP) %in% dvrpc)
pov[c(grep("1970", colnames(pov)))] <- NULL
pov[c(grep("1980", colnames(pov)))] <- NULL
pov[c(grep("125M", colnames(pov)))] <- NULL

# Separate by year
pov_90 <- pov
pov_90[c(grep("2000", colnames(pov_90)))] <- NULL
pov_90[c(grep("2012", colnames(pov_90)))] <- NULL
pov_90[c(grep("125", colnames(pov_90)))] <- NULL
pov_90 <- na.omit(pov_90)

pov_00 <- pov
pov_00[c(grep("1990", colnames(pov_00)))] <- NULL
pov_00[c(grep("2012", colnames(pov_00)))] <- NULL
pov_00[c(grep("125", colnames(pov_00)))] <- NULL
pov_00 <- na.omit(pov_00)

# 1990
pov_90 <- pov_90 %>%
  mutate(pov = AX6AA1990 - C20AI1990) %>%
  rename(above200 = C20AI1990)
tot_pov <- sum(pov_90$pov)
tot_above200 = sum(pov_90$above200)
pov_90 <- pov_90 %>%
  mutate(new_pov = pov / tot_pov,
         new_above200 = above200 / tot_above200,
         abs_diff = abs(new_pov - new_above200))
tot_diff_90 <- round((sum(pov_90$abs_diff) / 2), digits = 3)

# 2000
pov_00 <- pov_00 %>%
  mutate(pov = AX6AA2000 - C20AI2000) %>%
  rename(above200 = C20AI2000)
tot_pov <- sum(pov_00$pov)
tot_above200 = sum(pov_00$above200)
pov_00 <- pov_00 %>%
  mutate(new_pov = pov / tot_pov,
         new_above200 = above200 / tot_above200,
         abs_diff = abs(new_pov - new_above200))
tot_diff_00 <- round((sum(pov_00$abs_diff) / 2), digits = 3)

# 2010
collect_2$pov <- collect_2$povUniverse - collect_2$above200
tot_pov <- sum(collect_2$pov)
tot_above200 <- sum(collect_2$above200)
collect_2 <- collect_2 %>%
  mutate(new_pov = pov / tot_pov,
         new_above200 = above200 / tot_above200,
         abs_diff = abs(new_pov - new_above200))
tot_diff_10 <- round((sum(collect_2$abs_diff) / 2), digits = 3)

# 2017
collect$pov <- collect$povUniverse - collect$above200
tot_pov <- sum(collect$pov)
tot_above200 <- sum(collect$above200)
collect <- collect %>%
  mutate(new_pov = pov / tot_pov,
         new_above200 = above200 / tot_above200,
         abs_diff = abs(new_pov - new_above200))
tot_diff_17 <- round((sum(collect$abs_diff) / 2), digits = 3)

# Save and export
lowincome_dissim <- tribble(~Year, ~Idx,
                            1990, tot_diff_90,
                            2000, tot_diff_00,
                            2010, tot_diff_10,
                            2017, tot_diff_17)
write_csv(lowincome_dissim, "./outputs/low income dissimilarity indices.csv")
