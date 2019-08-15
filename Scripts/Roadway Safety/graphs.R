#KSI graphs
ksiState %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = ksiPerPop, color = state), lwd = 1) +
  geom_line(aes(y = ksiVulnerablePerPop, color = state), lwd = 1) +
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.0005, by = 0.00005), labels = seq(0, 5, by = 0.5)) +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Fatalities and Serious Injuries per 10,000 people")


ksiRegion %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = ksiPerPop)) +
  geom_col(aes(y = ksiVulnerablePerPop), fill = "red") +
  geom_line(aes(y = ksiPerPop5yrAvg), color = "blue", lwd = 2) +
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.0005, by = 0.00005), labels = seq(0, 5, by = 0.5)) +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Fatalities and Serious Injuries per 10,000 people")

ksiSubregion %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = ksiPerPop, color = subregion), lwd = 1) +
  geom_line(aes(y = ksiVulnerablePerPop, color = subregion), lwd = 1) +
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.0005, by = 0.00005), labels = seq(0, 5, by = 0.5)) +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Fatalities and Serious Injuries per 10,000 people")

ksiCounty %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = ksiPerPop, color = county), lwd = 1) +
  geom_line(aes(y = ksiVulnerablePerPop, color = county), lwd = 1) +
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.0005, by = 0.00005), labels = seq(0, 5, by = 0.5)) +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Fatalities and Serious Injuries per 10,000 people")


ksiSubregion %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = ksi, fill = subregion), position = "stack")