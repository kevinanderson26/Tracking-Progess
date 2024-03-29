library(here); library(dplyr); library(tidyr)
library(magrittr); library(ggplot2); library(car)

# Income by sex
# Note that this script isn't perfect: it assumes that all counties have
# sex parity in number of residents. Takes the total population and weights
# regional income by this number for expediency. Better to do size of labor force by sex.

l <- list.files(here("data", "acs"))

# sex x income files
lSex <- subset(l, substr(l, 12, 17) == "B20002")

# Inflation factors
inf <- read.csv("data/Inflation Adjustment/inflation.csv")
inf$year <- as.numeric(substr(inf$year, 3, 4))

# Qualifying counties
qct <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
sexInc <- data.frame()
for (i in 1:length(lSex)){
  temp <- read.csv(here("data", "acs", lSex[i]))
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lSex[i], 5, 6))
  sexInc <- rbind(sexInc, temp)
}
sexInc <- sexInc[c(1:3,6,8,10)]
sexInc$GEO.id2 <- as.character(sexInc$GEO.id2)

# add in Decennial
sexInc00 <- read.csv(here("data", "decennial", "DEC_00_SF3_P085_with_ann.csv"))
sexInc00 <- sexInc00[c(-4)]; colnames(sexInc00)[4:5] <- c("HD01_VD03", "HD01_VD04")
sexInc00$year <- 0; sexInc00 <- subset(sexInc00, GEO.id2 %in% qct)
sexInc <- rbind(sexInc, sexInc00)

pop00 <- read.csv(here("data", "decennial", "DEC_00_SF1_H009_with_ann.csv"))
pop00 <- pop00[-1,]; pop00 <- pop00[c(2,4)]; colnames(pop00)[2] <- "totPop"; pop00$year <- 0; pop00 <- subset(pop00, GEO.id2 %in% qct)

# Still need population for region means
lPop <- subset(l, substr(l, 12, 17) == "B01003")
pop <- data.frame()
for (i in 1:length(lPop)){
  temp <- read.csv(here("data", "acs", lPop[i]))
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lPop[i], 5, 6))
  pop <- rbind(pop, temp)
}
pop <- pop[c("GEO.id2", "year", "HD01_VD01")]
colnames(pop)[3] <- "totPop"
pop$GEO.id2 <- as.character(pop$GEO.id2)
pop <- rbind(pop, pop00)

sexInc %<>% left_join(pop, by = c("year", "GEO.id2")) %>%
  left_join(inf, by = c("year")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_at(c(4:8), as.numeric) %>%
  mutate_at(c(2), as.factor) %>%
  gather(sex, income, HD01_VD03, HD01_VD04) %>%
  mutate(incomeAdj = income * inf_factor) %>%
  select(GEO.id2, year, totPop, sex, incomeAdj)

# append suburban county averages and regional averages
t2 <- filter(sexInc, GEO.id2 %in% c(42017, 42029, 42045, 42091)) %>%
  group_by(year, sex) %>%
  summarize(GEO.id2 = "PA Suburban Counties",
            incomeAdj = weighted.mean(incomeAdj, totPop),
            totPop = sum(totPop))
t3 <- filter(sexInc, GEO.id2 %in% c(34005, 34007, 34015, 34021)) %>%
  group_by(year, sex) %>%
  summarize(GEO.id2 = "NJ Suburban Counties",
            incomeAdj = weighted.mean(incomeAdj, totPop),
            totPop = sum(totPop))
t4 <- sexInc %>%
  group_by(year, sex) %>%
  summarize(GEO.id2 = "Region",
            incomeAdj = weighted.mean(incomeAdj, totPop),
            totPop = sum(totPop))

final <- bind_rows(sexInc, t2) %>%
  bind_rows(., t3) %>%
  bind_rows(., t4) %>%
  mutate_if(is.numeric, ~round(., 0))

final$year <- recode(final$year,"0=2000;5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")

colnames(final)[1] <- "geography"; final$geography <- as.factor(final$geography)

final$geography <- recode(
  final$geography,
  "34005='Burlington';34007='Camden';34015='Gloucester';34021='Mercer';
  42017='Bucks';42029='Chester';42045='Delaware';42091='Montgomery';42101='Philadelphia'")

final$sex <- recode(final$sex, "'HD01_VD03'='Male';'HD01_VD04'='Female'")

# sample graph to show Ben
t1 <- filter(final, geography == "Region")
ggplot(data = t1, aes(x = year, y = incomeAdj)) +
  geom_line(aes(color = sex)) +
  labs(title = "Sample graph of income by sex",
       y = "Annual median income, 2017 USD",
       x = "Year")

final %<>% spread(sex, incomeAdj) %>%
  mutate(difference = Male - Female,
         ratio = Female / Male * 100)

write.csv(final[c(-3)], here("outputs", "sexInc_v2.csv"), row.names = FALSE)

t1 <- filter(final, geography == "Region")
ggplot(data = t1, aes(x = year, y = difference)) +
  geom_line() +
  labs(title = "Difference in male and female income",
       y = "Difference in annual median income, 2017 USD",
       x = "Year")

# transform data for web applications
wide1 <- spread(final, geography, Male) %>%
  select(-c(2:5)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_MaleMed", .)) %>%
  mutate(year = c(2000,2005:2017))

wide2 <- spread(final, geography, Female) %>%
  select(-c(2:5)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_FemaleMed", .)) %>%
  mutate(year = c(2000,2005:2017))

wide3 <- spread(final, geography, difference) %>%
  select(-c(2:5)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_difference", .)) %>%
  mutate(year = c(2000,2005:2017))

wide4 <- spread(final, geography, ratio) %>%
  select(-c(2:5)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_ratio", .)) %>%
  mutate(year = c(2000,2005:2017))

res <- left_join(wide1, wide2, by = "year") %>%
  left_join(., wide3, by = "year") %>%
  left_join(., wide4, by = "year")

write.csv(res, here("outputs", "sexInc_wide_v2.csv"), row.names = FALSE)
