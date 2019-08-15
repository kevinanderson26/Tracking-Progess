library(tidycensus); library(tidyverse)

# Study area FIPS codes
dvrpc <- c("34005", "34007", "34015", "34021",
           "42017", "42029", "42045", "42091", "42101")

# API Calls for all 2010 and 2017 variables

#API Key (tied to KAnderson@dvrpc.org, may need to be changed in the future)
census_api_key("9999d8f1945ed47effb2dabd2dcad9af8213909f", overwrite = FALSE, install = TRUE)


# Check if 2017 data exists
# If not, download from API
if(!file.exists("./data/api_2017.csv")){
  # https://api.census.gov/data/2017/acs/acs5/variables.html
  collect <- get_acs(geography = "tract", year = 2017, state = c(34,42), output = "wide",
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

# Racial dissimilarity index
# For formula see https://www.dartmouth.edu/~segregation/IndicesofSegregation.pdf
race <- read_csv("./data/nhgis0002_ts_geog2010_tract.csv") %>%
  mutate_at(vars(COUNTYA), funs(str_pad(., width = 3, side = "left", pad = "0"))) %>%
  filter(paste0(STATEA, COUNTYA) %in% dvrpc)

# Remove unneeded columns
remove <- c(grep("\\L$", colnames(race)), grep("\\U$", colnames(race)))
race[c(remove)] <- NULL

# Separate by year
race90 <- race
race90[c(grep("2000", colnames(race90)))] <- NULL
race90[c(grep("2010", colnames(race90)))] <- NULL
race00 <- race
race00[c(grep("1990", colnames(race00)))] <- NULL
race00[c(grep("2010", colnames(race00)))] <- NULL

race90$tot_pop <- rowSums(race90[,8:13])
race00$tot_pop <- rowSums(race00[,8:13])

# White-Nonwhite
race90$non_wht <- rowSums(race90[,9:13])
race00$non_wht <- rowSums(race00[,9:13])

# 1990
tot_wht <- sum(race90$CY8AA1990)
tot_nonwht <- sum(race90$non_wht)

race90 <- race90 %>%
  mutate(new_wht = CY8AA1990 / tot_wht,
         new_nonwht = non_wht / tot_nonwht,
         abs_diff = abs(new_wht - new_nonwht))
tot_diff_90 <- round((sum(race90$abs_diff) / 2), digits = 3)

# 2000
tot_wht <- sum(race00$CY8AA2000)
tot_nonwht <- sum(race00$non_wht)
race00 <- race00 %>%
  mutate(new_wht = CY8AA2000 / tot_wht,
         new_nonwht = non_wht / tot_nonwht,
         abs_diff = abs(new_wht - new_nonwht))
tot_diff_00 <- round((sum(race00$abs_diff) / 2), digits = 3)

# 2010
collect_2$non_wht <- collect_2$racUniverse - collect_2$racWhite
tot_wht <- sum(collect_2$racWhite)
tot_nonwht <- sum(collect_2$non_wht)
collect_2 <- collect_2 %>%
  mutate(new_wht = racWhite / tot_wht,
         new_nonwht = non_wht / tot_nonwht,
         abs_diff = abs(new_wht - new_nonwht))
tot_diff_10 <- round((sum(collect_2$abs_diff) / 2), digits = 3)

# 2017
collect$non_wht <- collect$racUniverse - collect$racWhite
tot_wht <- sum(collect$racWhite)
tot_nonwht <- sum(collect$non_wht)
collect <- collect %>%
  mutate(new_wht = racWhite / tot_wht,
         new_nonwht = non_wht / tot_nonwht,
         abs_diff = abs(new_wht - new_nonwht))
tot_diff_17 <- round((sum(collect$abs_diff) / 2), digits = 3)

# Save and export
racial_dissim <- tribble(~Year, ~Idx,
                         1990, tot_diff_90,
                         2000, tot_diff_00,
                         2010, tot_diff_10,
                         2017, tot_diff_17)
write_csv(racial_dissim, "./outputs/racial dissimilarity indices_2017.csv")

# Hispanic-Nonhispanic
race90$nonHisp <- rowSums(race90[,8:12])
race00$nonHisp <- rowSums(race00[,8:12])
# race2010$nonHisp <- rowSums(race2010[,8:12])

# 1990
tot_hisp <- sum(race90$CY8AF1990)
tot_nonhisp <- sum(race90$nonHisp)
race_90 <- race90 %>%
  mutate(new_hisp = CY8AF1990 / tot_hisp,
         new_nonhisp = nonHisp / tot_nonhisp,
         abs_diff = abs(new_hisp - new_nonhisp))
tot_diff_90 <- round((sum(race_90$abs_diff) / 2), digits = 3)

# 2000
tot_hisp <- sum(race00$CY8AF2000)
tot_nonhisp <- sum(race00$nonHisp)
race_00 <- race00 %>%
  mutate(new_hisp = CY8AF2000 / tot_hisp,
         new_nonhisp = nonHisp / tot_nonhisp,
         abs_diff = abs(new_hisp - new_nonhisp))
tot_diff_00 <- round((sum(race_00$abs_diff) / 2), digits = 3)

# 2010
collect_2$hisp <- collect_2$hispUniverse - collect_2$notHisp
tot_hisp <- sum(collect_2$hisp)
tot_nonhisp <- sum(collect_2$notHisp)
collect_2 <- collect_2 %>%
  mutate(new_hisp = hisp / tot_hisp,
        new_nonhisp = notHisp / tot_nonhisp,
        abs_diff = abs(new_hisp - new_nonhisp))
tot_diff_10 <- round((sum(collect_2$abs_diff) / 2), digits = 3)

# 2017
collect$hisp <- collect$hispUniverse - collect$notHisp
tot_hisp <- sum(collect$hisp)
tot_nonhisp <- sum(collect$notHisp)
collect <- collect %>%
  mutate(new_hisp = hisp / tot_hisp,
         new_nonhisp = notHisp / tot_nonhisp,
         abs_diff = abs(new_hisp - new_nonhisp))
tot_diff_17 <- round((sum(collect$abs_diff) / 2), digits = 3)

# Save and export
hispanic_dissim <- tribble(~Year, ~Idx,
                           1990, tot_diff_90,
                           2000, tot_diff_00,
                           2010, tot_diff_10,
                           2017, tot_diff_17)
write_csv(hispanic_dissim, "./outputs/hispanic dissimilarity indices_2017.csv")

# White Nonhispanic-Other
# Can only do this for 2010 and 2017
# Because Census questions change over time

# 2010
collect_2$other <- collect_2$hispUniverse - collect_2$whiteNotHisp
tot_oth <- sum(collect_2$other)
tot_whtnothisp <- sum(collect_2$whiteNotHisp)
collect_2 <- collect_2 %>%
  mutate(new_oth = other / tot_oth,
         new_whtnothisp = whiteNotHisp / tot_whtnothisp,
         abs_diff = abs(new_whtnothisp - new_oth))
tot_diff_10 <- round((sum(collect_2$abs_diff) / 2), digits = 3)

# 2017
collect$other <- collect$hispUniverse - collect$whiteNotHisp
tot_oth <- sum(collect$other)
tot_whtnothisp <- sum(collect$whiteNotHisp)
collect <- collect %>%
  mutate(new_oth = other / tot_oth,
         new_whtnothisp = whiteNotHisp / tot_whtnothisp,
         abs_diff = abs(new_whtnothisp - new_oth))
tot_diff_17 <- round((sum(collect$abs_diff) / 2), digits = 3)

# Save and export
whtnothispanic_dissim <- tribble(~Year, ~Idx,
                           2010, tot_diff_10,
                           2017, tot_diff_17)
write_csv(whtnothispanic_dissim, "./outputs/white not hispanic dissimilarity indices_2017.csv")
