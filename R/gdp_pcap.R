# GDP per Capita Analysis

mean_gdp <- combined_data %>% 
  group_by(country) %>% 
  summarise(mean_gdp = mean(gdp_pcap_num, na.rm = TRUE),
            pop = mean(pop_num, na.rm = TRUE)) 

highest_mean_gdp_country <- mean_gdp %>% 
  arrange(-mean_gdp)%>% 
  head(1) %>% 
  pull(country)

highest_mean_gdp_value <- mean_gdp %>% 
  arrange(-mean_gdp)%>%
  head(1) %>%
  pull(mean_gdp) %>% 
  round()

lowest_mean_gdp_country <- mean_gdp %>% 
  arrange(mean_gdp)%>% 
  head(1) %>% 
  pull(country)

lowest_mean_gdp_value <- mean_gdp %>% 
  arrange(mean_gdp) %>%
  head(1) %>%
  pull(mean_gdp)%>% 
  round()

average_gdp_value <- mean_gdp %>%
  summarise(weighted_mean_gdp = sum(mean_gdp * pop, na.rm = T) / sum(pop, na.rm = T)) %>%   
  round(2)


top_10_country_gdp <- mean_gdp %>% 
  arrange(-mean_gdp) %>% 
  head(10) %>%
  mutate(tooltip_label = paste(country, 
                               round(mean_gdp, 1),
                               sep = ": ")) %>% 
  ggplot(mapping = aes(y = reorder(country, mean_gdp), x = mean_gdp, fill = mean_gdp,
                       text = tooltip_label)) +
  geom_col() +
  geom_text(aes(label = round(mean_gdp, 1)), 
            nudge_x = -25,
            color = "white") +
  labs(x = "Top 10 Mean GDP per Capita",
       y = "Country") + 
  theme(legend.position = "none")

top_20_country_gdp_ggplotly <- ggplotly(top_10_country_gdp, tooltip = "text")


bottom_10_country_gdp <- mean_gdp %>% 
  arrange(mean_gdp) %>% 
  head(10) %>%
  mutate(tooltip_label = paste(country, 
                               round(mean_gdp, 1),
                               sep = ": ")) %>% 
  ggplot(mapping = aes(y = reorder(country, mean_gdp), x = mean_gdp, fill = mean_gdp,
                       text = tooltip_label)) +
  geom_col() +
  geom_text(aes(label = round(mean_gdp, 1)), 
            nudge_x = -3,
            color = "white") +
  labs(x = "Bottom 10 Mean GDP per Capita",
       y = "Country") + 
  theme(legend.position = "none")

bottom_10_country_gdp_ggplotly <- ggplotly(bottom_10_country_gdp, tooltip = "text")

data_gdp <- mean_gdp %>% 
  mutate(`mean gdp per capita over time` = round(mean_gdp)) %>% 
  select(country, `mean gdp per capita over time`) %>% 
  reactable::reactable(., sortable = T, filterable = T, searchable = T)


# Join the country polygons with your dataset by ISO3 codes
world_gdp_data <- left_join(world, 
                           mean_gdp %>% 
                             mutate(`mean gdp per capita over time` = round(mean_gdp)) %>% 
                             select(country, `mean gdp per capita over time`) %>% 
                             mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")),
                           by = c("iso_a3" = "iso3c"))

gdp_map <- world_gdp_data %>% 
  mutate(tooltip_label = paste(country, 
                               `mean gdp per capita over time`,
                               sep = ": ")) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = `mean gdp per capita over time`, text = tooltip_label)) +
  theme_void() +
  theme(legend.position = "none")

gdp_map_ggplotly <- ggplotly(gdp_map, tooltip = "text")