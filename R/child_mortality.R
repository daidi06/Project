# Child mortality Analysis

mean_cm <- combined_data %>% 
  group_by(country) %>% 
  summarise(mean_cm = mean(child_mortality_num, na.rm = TRUE),
            pop = mean(pop_num, na.rm = TRUE)) 

highest_mean_cm_country <- mean_cm %>% 
  arrange(-mean_cm)%>% 
  head(1) %>% 
  pull(country)

highest_mean_cm_value <- mean_cm %>% 
  arrange(-mean_cm)%>%
  head(1) %>%
  pull(mean_cm) %>% 
  round()

lowest_mean_cm_country <- mean_cm %>% 
  arrange(mean_cm)%>% 
  head(1) %>% 
  pull(country)

lowest_mean_cm_value <- mean_cm %>% 
  arrange(mean_cm) %>%
  head(1) %>%
  pull(mean_cm)%>% 
  round()

average_cm_value <- mean_cm %>%
  summarise(weighted_mean_cm = sum(mean_cm * pop, na.rm = T) / sum(pop, na.rm = T)) %>%   
  round(2)


top_10_country_cm <- mean_cm %>% 
  arrange(-mean_cm) %>% 
  head(10) %>%
  mutate(tooltip_label = paste(country, 
                               round(mean_cm, 1),
                               sep = ": ")) %>% 
  ggplot(mapping = aes(y = reorder(country, mean_cm), x = mean_cm, fill = mean_cm,
                       text = tooltip_label)) +
  geom_col() +
  geom_text(aes(label = round(mean_cm, 1)), 
            nudge_x = -25,
            color = "white") +
  labs(x = "Top 10 Mean Child Mortality",
       y = "Country") + 
  theme(legend.position = "none")

top_20_country_cm_ggplotly <- ggplotly(top_10_country_cm, tooltip = "text")


bottom_10_country_cm <- mean_cm %>% 
  arrange(mean_cm) %>% 
  head(10) %>%
  mutate(tooltip_label = paste(country, 
                               round(mean_cm, 1),
                               sep = ": ")) %>% 
  ggplot(mapping = aes(y = reorder(country, mean_cm), x = mean_cm, fill = mean_cm,
                       text = tooltip_label)) +
  geom_col() +
  geom_text(aes(label = round(mean_cm, 1)), 
            nudge_x = -3,
            color = "white") +
  labs(x = "Bottom 10 Mean Child Mortality",
       y = "Country") + 
  theme(legend.position = "none")

bottom_10_country_cm_ggplotly <- ggplotly(bottom_10_country_cm, tooltip = "text")

data_cm <- mean_cm %>% 
  mutate(`mean child mortality over time` = round(mean_cm)) %>% 
  select(country, `mean child mortality over time`) %>% 
  reactable::reactable(., sortable = T, filterable = T, searchable = T)


# Join the country polygons with your dataset by ISO3 codes
world_cm_data <- left_join(world, 
                           mean_cm %>% 
                             mutate(`mean child mortality over time` = round(mean_cm)) %>% 
                             select(country, `mean child mortality over time`) %>% 
                             mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")),
                           by = c("iso_a3" = "iso3c"))

cm_map <- world_cm_data %>% 
  mutate(tooltip_label = paste(country, 
                               `mean child mortality over time`,
                               sep = ": ")) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = `mean child mortality over time`, text = tooltip_label)) +
  theme_void() +
  theme(legend.position = "none")

cm_map_ggplotly <- ggplotly(cm_map, tooltip = "text")