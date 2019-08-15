ksiSubregion %>% filter(subregion == "Philadelphia") %>%
ggplot(aes(x = year)) +
  geom_col(aes(y = ksi)) +
  geom_col(aes(y = ksiBikePed), fill = "red") +
  geom_path(aes(y = ksi5yrAvg), lwd = 1, color = "blue") +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ksiCounty %>% filter(county == "Montgomery") %>%
ggplot(aes(x = year)) +
  geom_col(aes(y = ksiPerPop)) +
  geom_col(aes(y = ksiBikePedPerPop), fill = "red") +
  geom_path(aes(y = ksiPerPop5yrAvg), lwd = 1, color = "blue") +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ksiSubregion %>% filter(subregion == "Philadelphia") %>%
ggplot(aes(x = year)) +
  geom_col(aes(y = ksiPerVMT)) +
  geom_path(aes(y = ksiPerVMT5yrAvg), lwd = 1, color = "blue") +
  scale_x_continuous(breaks = seq(2006, 2016, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))