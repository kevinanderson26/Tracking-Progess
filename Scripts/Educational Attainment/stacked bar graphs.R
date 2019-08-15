edattain5yr %>% filter(year == 2000) %>% group_by(planningArea, edCategory) %>% summarize(n = sum(n)) %>%
  ggplot() +
  geom_col(aes(x = planningArea, y = n, fill = edCategory), position = "fill") +
  ggtitle("Educational Attainment by Planning Area, 2000")

edattain5yr %>% filter(year == 2009) %>% group_by(planningArea, edCategory) %>% summarize(n = sum(n)) %>%
  ggplot() +
  geom_col(aes(x = planningArea, y = n, fill = edCategory), position = "fill") +
  ggtitle("Educational Attainment by Planning Area, 2009")

edattain5yr %>% filter(year == 2016) %>% group_by(planningArea, edCategory) %>% summarize(n = sum(n)) %>%
  ggplot() +
  geom_col(aes(x = planningArea, y = n, fill = edCategory), position = "fill") +
  ggtitle("Educational Attainment by Planning Area, 2016")


edPA2000 <- edattain5yr %>% filter(year == 2000) %>% group_by(planningArea, edCategory) %>% summarize(n = sum(n)) %>% mutate(total = sum(n), percent = n/total) %>% select(-n, -total) %>% spread(edCategory, percent)