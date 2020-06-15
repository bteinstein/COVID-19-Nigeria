### Summaries
Zone_Summary <- df_zones %>% 
  group_by(Geopolitical_Zone) %>% 
  summarise(region_sum = sum(Confirmed_Cases_cum)) %>% 
  ungroup() %>% 
  mutate(pct_region_sum = (region_sum/sum(region_sum))*100) %>% 
  arrange(-pct_region_sum)

glue::glue(Zone_Summary$Geopolitical_Zone, ({Zone_Summary$region_sum})
           