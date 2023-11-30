
## Please load in data associated with LML_P1_community-response.R to get the BEF_data and v data frame

## Sites sampled in May vs. June in 2012

BEF_data %>% 
  filter(SPECIES == i) %>%
  filter(YEAR == 2012) %>% 
  select(SITE, MONTH) %>% 
  unique() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = MONTH, values_from = value)

 for(i in species){
  
  graph = BEF_data %>% 
    filter(SPECIES == i) %>%
    filter(YEAR %in% c(2009:2014)) %>%
    mutate(MONTH_binned = .bincode(MONTH, month_bin)) %>% 
    group_by(YEAR, SPECIES, SITE, MONTH,MONTH_binned, DSAMP_N, EFFORT) %>% 
    summarize(CPUE_min = n()) %>% 
    mutate(CPUE_min = (CPUE_min / EFFORT)*60) %>%
    ggplot(aes(x = (YEAR),
               y = CPUE_min,
              col = as.factor(MONTH_binned))) + 
    geom_point() + 
    facet_wrap(~SITE) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(col = paste(i, "Month Sampled")) +
    theme_minimal()
  print(graph)
}
 


v %>%
 
  filter(Year %in% c(2010:2014)) %>% ggplot(aes(x = Year, y = value)) +
  geom_point() +
  facet_wrap(~Species, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90)) 
