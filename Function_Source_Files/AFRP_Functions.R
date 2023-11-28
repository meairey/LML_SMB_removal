fish = read.csv("MA2276_Code/Data/FISH_MEASUREMENT_2022.csv")
sample = read.csv("MA2276_Code/Data/FISH_SAMPLE_2022.csv")
sites = read.csv("MA2276_Code/Data/SITES.csv")
shoreline_length = read.csv("MA2276_Code/Data/BEFsites_LengthAndHabitat.csv")

species = unique(fish$SPECIES)

filter_data = function(water, 
                       gear, 
                       species ,
                       gear_code,
                       min_year = 1900, 
                       max_year = 2100, 
                       min_month = 0,
                       max_month = 13){
  left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  left_join(shoreline_length, by = "SITE_N") %>%
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>% 
  filter(WATER %in% water,
         GEAR %in% gear,
           SPECIES != "NF", 
           SPECIES != "",
           SPECIES %in% species, 
           HAB_1 != "NA",
           HAB_1 != "",
           GEAR_CODE %in% gear_code, 
           YEAR > min_year , 
           YEAR < max_year, 
           MONTH > min_month,
           MONTH < max_month)
}





hab_numbs = function(input_data){
  Hab_numbs = input_data %>%
    select(HAB_1, SITE) %>%
    unique() %>%
    count(HAB_1) %>%
    rename(HAB_NUMB = n)
   
  return(Hab_numbs)
}

CPUE_long_seconds = function(data_input){
  data_input %>% select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
                        FISH_N, WEIGHT, LENGTH,  EFFORT) %>%
    group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT) %>%
    count() %>% ## Abundance per year, site, species
    mutate(CPUE_seconds = n / EFFORT) %>%
    ungroup() %>%
    complete(WATER, YEAR,DAY_N, SITE,SPECIES) %>%
    replace_na(list(CPUE_seconds = 0, n = 0))
}

CPUE_long_seconds_habitat = function(data_input){
  data_input %>% select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
                        FISH_N, WEIGHT, LENGTH, HAB_1, GEAR, EFFORT) %>%
    group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT, HAB_1) %>%
    count() %>% ## Abundance per year, site, species
    left_join(Hab_numbs, by = "HAB_1") %>%
    mutate(CPUE_std = n / (EFFORT*HAB_NUMB)) %>%
    ungroup() %>%
    complete(WATER, YEAR,DAY_N, SITE,SPECIES) %>%
    replace_na(list(CPUE_seconds = 0, n = 0))
}


## CPUE_wide does not average across habitat
CPUE_wide_seconds_avg = function(data_input){
  cat =   data_input %>%
    select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
           FISH_N, WEIGHT, LENGTH, HAB_1, GEAR, EFFORT) %>%
    group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT, HAB_1) %>%
    count() %>% ## Abundance per year, site, species
    ungroup() %>%
    complete(., nesting(WATER, YEAR, DAY_N, SITE, EFFORT, HAB_1), SPECIES) %>%
    replace_na(list(CPUE_seconds = 0, n = 0)) %>%
    mutate(CPUE_seconds = n / EFFORT) %>%
    select(-n) %>%
    mutate(CPUE_seconds = replace_na(CPUE_seconds,0)) %>%
    as.data.frame() %>% select(SPECIES,
                               CPUE_seconds, 
                               SITE, DAY_N, YEAR) %>% 
      group_by(YEAR, SPECIES) %>% 
      summarise(cpue = mean(CPUE_seconds)) %>%
      pivot_wider(names_from = SPECIES, values_from = cpue)
}

## CPUE_wide_seconds not averaged across sites
CPUE_wide_seconds = function(data_input){ ## I removed HAB_1 from the select for the TPN data
  data_input %>%
    select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
           FISH_N, WEIGHT, LENGTH,  EFFORT) %>%
    group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT) %>%
    count() %>% ## Abundance per year, site, species
    mutate(CPUE_seconds = n / EFFORT) %>%
    ungroup() %>%
    complete(., nesting(WATER, YEAR, DAY_N, SITE, EFFORT), SPECIES) %>%
    replace_na(list(CPUE_seconds = 0, n = 0)) %>%
    select(-n) %>%
    as.data.frame() %>% select(SPECIES,
                               CPUE_seconds, 
                               SITE, DAY_N, YEAR) %>% 
    group_by(YEAR, SITE, SPECIES) %>%
    summarise(cpue = mean(CPUE_seconds)) %>%
    pivot_wider(names_from = SPECIES, values_from = cpue)
}


CPUE_wide_shore = function(data_input){
  data_input %>%
  select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
         WEIGHT, LENGTH, HAB_1, GEAR, EFFORT, Shape_Length) %>%
  group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT, HAB_1, Shape_Length) %>%
  count() %>% ## Abundance per year, site, species
  mutate(CPUE_shoreline = n / Shape_Length) %>%
  ungroup() %>%
  complete(WATER, YEAR,DAY_N, SITE,SPECIES) %>%
  replace_na(list(CPUE_shoreline = 0, n = 0)) %>%
  select(-n) %>%
  pivot_wider(names_from = SPECIES, values_from = CPUE_shoreline) %>%
  mutate(across(everything(), ~replace_na(.x,0)))
}

CPUE_long_shore = function(data_input){
  data_input %>% 
    select(YSAMP_N, DAY_N, YEAR, SEASON, 
           WATER, SITE, SPECIES,
           FISH_N, WEIGHT, LENGTH,
           HAB_1, GEAR, EFFORT, Shape_Length) %>%
    group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT,Shape_Length) %>%
    count() %>% ## Abundance per year, site, species
    mutate(CPUE_shore = n / Shape_Length) %>%
    ungroup() %>%
    complete(WATER, YEAR,DAY_N, SITE,SPECIES) %>%
    replace_na(list(CPUE_shore = 0, n = 0))
}


