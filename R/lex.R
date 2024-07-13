# Life Expentancy Analysis

mean_lex <- combined_data %>% 
  group_by(country) %>% 
  summarise(mean_lex = mean(lex_num, na.rm = TRUE),
            pop = mean(pop_num, na.rm = TRUE)) 

highest_mean_lex_country <- mean_lex %>% 
  arrange(-mean_lex)%>% 
  head(1) %>% 
  pull(country)

highest_mean_lex_value <- mean_lex %>% 
  arrange(-mean_lex)%>%
  head(1) %>%
  pull(mean_lex) %>% 
  round()

lowest_mean_lex_country <- mean_lex %>% 
  arrange(mean_lex)%>% 
  head(1) %>% 
  pull(country)

lowest_mean_lex_value <- mean_lex %>% 
  arrange(mean_lex) %>%
  head(1) %>%
  pull(mean_lex)%>% 
  round()

average_lex_value <- mean_lex %>%
  summarise(weighted_mean_lex = sum(mean_lex * pop, na.rm = T) / sum(pop, na.rm = T)) %>%   
  round(2)


top_10_country_lex <- mean_lex %>% 
  arrange(-mean_lex) %>% 
  head(10) %>%
  mutate(tooltip_label = paste(country, 
                               round(mean_lex, 1),
                               sep = ": ")) %>% 
  ggplot(mapping = aes(y = reorder(country, mean_lex), x = mean_lex, fill = mean_lex,
                       text = tooltip_label)) +
  geom_col() +
  geom_text(aes(label = round(mean_lex, 1)), 
            nudge_x = -25,
            color = "white") +
  labs(x = "Top 10 Mean Life Expentancy",
       y = "Country") + 
  theme(legend.position = "none")

top_20_country_lex_ggplotly <- ggplotly(top_10_country_lex, tooltip = "text")


bottom_10_country_lex <- mean_lex %>% 
  arrange(mean_lex) %>% 
  head(10) %>%
  mutate(tooltip_label = paste(country, 
                               round(mean_lex, 1),
                               sep = ": ")) %>% 
  ggplot(mapping = aes(y = reorder(country, mean_lex), x = mean_lex, fill = mean_lex,
                       text = tooltip_label)) +
  geom_col() +
  geom_text(aes(label = round(mean_lex, 1)), 
            nudge_x = -3,
            color = "white") +
  labs(x = "Bottom 10 Mean Life Expentancy",
       y = "Country") + 
  theme(legend.position = "none")

bottom_10_country_lex_ggplotly <- ggplotly(bottom_10_country_lex, tooltip = "text")

data_lex <- mean_lex %>% 
  mutate(`mean lex over time` = round(mean_lex)) %>% 
  select(country, `mean lex over time`) %>% 
  reactable::reactable(., sortable = T, filterable = T, searchable = T)


# Join the country polygons with your dataset by ISO3 codes
world_lex_data <- left_join(world, 
                           mean_lex %>% 
                             mutate(`mean lex over time` = round(mean_lex)) %>% 
                             select(country, `mean lex over time`) %>% 
                             mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")),
                           by = c("iso_a3" = "iso3c"))

lex_map <- world_lex_data %>% 
  mutate(tooltip_label = paste(country, 
                               `mean lex over time`,
                               sep = ": ")) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = `mean lex over time`, text = tooltip_label)) +
  theme_void() +
  theme(legend.position = "none")

lex_map_ggplotly <- ggplotly(lex_map, tooltip = "text")