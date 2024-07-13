
# Create the plot
p <- world_data %>% 
  mutate(year = lubridate::year(ymd(paste0(year, "-01-01")))) %>% 
  ggplot() +
  geom_sf(aes(fill = gdp_pcap_num)) +
  scale_fill_viridis_c() +
  labs(title = 'Year: {frame_time}', fill = 'Value') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  transition_time(year) +
  ease_aes('linear')

# Animate and save
gdp_anim <- animate(p, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer())
gganimate::anim_save(here("images/gdp_animated_map.gif"), animation = gdp_anim)
