#map theme
map_theme <- theme(legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   axis.text=element_text(size=5),
                   axis.title=element_text(size=6))



#rail vehicle stacked bar
sum_table_gathered %>%
  filter(class == "Rail Vehicles") %>%
  ggplot() +
  map_theme +
  geom_col(aes(x = year, y = n_in_cat, fill = age_cat), 
           position = position_stack(reverse = TRUE)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Age of Rail Vehicles", fill = "Vehicle Age", x = "Year", y = "Number of Vehicles") +
  scale_fill_hue(labels = c("0-5 years", "6-11 years", "12-15 years", 
                            "16-20 years", "21-25 years", "More than 25 years"))

ggsave("rail age.png", width = 6, height = 3, dpi = "print")

#bus stacked bar
sum_table_gathered %>%
  filter(class == "Buses") %>%
  ggplot() +
  map_theme +
  geom_col(aes(x = year, y = n_in_cat, fill = age_cat), 
           position = position_stack(reverse = TRUE)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Age of Buses", fill = "Vehicle Age", x = "Year", y = "Number of Vehicles") +
  scale_fill_hue(labels = c("0-5 years", "6-11 years", "12-15 years", 
                            "16-20 years", "21-25 years", "More than 25 years"))

ggsave("bus age.png", width = 6, height = 3, dpi = "print")

#average age line chart
avg_age <- vehicleAge %>% filter(class != "Other") %>%
  group_by(year, class) %>%
  summarize(average_age = weighted.mean(avgAge, nVehicles))

avg_age_total <- vehicleAge %>% filter(class != "Other") %>%
  group_by(year) %>%
  summarize(average_age = weighted.mean(avgAge, nVehicles),
         class = "All Vehicles") %>%
  bind_rows(avg_age)

ggplot(avg_age_total) +
  geom_line(aes(x = year, y = average_age, color = class)) + 
  scale_y_continuous(limits = c(0,25))