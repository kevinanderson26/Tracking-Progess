#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl")
pack(packages)
rm(pack, packages)

#source processed data
source("data_processing.R")

#total VMT graphs
ggplot(vmtAllDVRPC, aes(x = year, y = milesAnnual)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 45000000000, by = 5000000000), 
                     labels = seq(0, 45, by = 5)) +
  ylab("Annual VMT (billions)") +
  ggtitle("Annual VMT, DVRPC Region")
#ggsave("../annualVMTregion.png")

ggplot(vmtSuburbs, aes(x = year, y = milesAnnual, color = region)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 25000000000, by = 5000000000), 
                     labels = seq(0, 25, by = 5)) +
  ylab("Annual VMT (billions)") +
  ggtitle("Annual VMT, DVRPC Subregions")
#ggsave("../annualVMTsubregion.png")

ggplot(vmtCounties, aes(x = year, y = milesAnnual, color = county)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 45000000000, by = 1000000000), 
                     labels = seq(0, 45, by = 1)) +
  ylab("Annual VMT (billions)") +
  ggtitle("Annual VMT, DVRPC Counties")
#ggsave("../annualVMTcounty.png")

#VMT per capita graphs
ggplot(vmtAllDVRPC, aes(x = year, y = milesPerCapita)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 100000, by = 1000), 
                     labels = seq(0, 100, by = 1)) +
  ylab("Annual VMT (thousands)") +
  ggtitle("VMT per Capita, DVRPC Region")
#ggsave("../percapVMTregion.png")

ggplot(vmtSuburbs, aes(x = year, y = milesPerCapita, color = region)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 100000, by = 1000), 
                     labels = seq(0, 100, by = 1)) +
  ylab("Annual VMT (thousands)") +
  ggtitle("VMT per Capita, DVRPC Subegions")
#ggsave("../percapVMTsubregion.png")

ggplot(vmtCounties, aes(x = year, y = milesPerCapita, color = county)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 100000, by = 1000), 
                     labels = seq(0, 100, by = 1)) +
  ylab("Annual VMT (thousands)") +
  ggtitle("VMT per Capita, DVRPC Counties")
#ggsave("../percapVMTcounty.png")

#VMT per vehicle graphs
vmtAllDVRPC %>% filter(!is.na(milesPerVehicle)) %>%
ggplot(aes(x = year, y = milesPerVehicle)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 100000, by = 1000), 
                     labels = seq(0, 100, by = 1)) +
  ylab("Annual VMT (thousands)") +
  ggtitle("VMT per Vehicle, DVRPC Region")
#ggsave("../pervehicleVMTregion.png")

vmtSuburbs %>% filter(!is.na(milesPerVehicle)) %>%
ggplot(aes(x = year, y = milesPerVehicle, color = region)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 100000, by = 1000), 
                     labels = seq(0, 100, by = 1)) +
  ylab("Annual VMT (thousands)") +
  ggtitle("VMT per Vehicle, DVRPC Subregions")
#ggsave("../pervehicleVMTsubregion.png")

vmtCounties %>% filter(!is.na(milesPerVehicle)) %>%
ggplot(aes(x = year, y = milesPerVehicle, color = county)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = seq(0, 100000, by = 1000), 
                     labels = seq(0, 100, by = 1)) +
  ylab("Annual VMT (thousands)") +
  ggtitle("VMT per Vehicle, DVRPC Counties")
#ggsave("../pervehicleVMTcounty.png")