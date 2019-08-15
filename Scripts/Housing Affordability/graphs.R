ratio_PA %>%
  ggplot(aes(x = year, y = ratio)) +
  geom_line(aes(color = planning_area, linetype = type)) +
  scale_y_continuous(limits = c(0, 1))

ratio_region %>% 
  ggplot(aes(x = year, y = total)) +
  geom_col(aes(fill = type), position = "fill")

ratio_region %>% group_by(year) %>% summarize(expensive = sum(expensive), total = sum(total)) %>% mutate(ratio = expensive / total) %>% 
  ggplot(aes(x = year, y = expensive)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 500000))

ratio_region %>% group_by(year) %>% summarize(expensive = sum(expensive), total = sum(total)) %>% mutate(ratio = expensive / total) %>% 
  ggplot(aes(x = year, y = ratio)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1))