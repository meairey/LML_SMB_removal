## Assessing impacts of year 2000 depletion through time on SMB populations 

library(tidyr)
library(tidyverse)
## Overall Depletion 

##It looks like there is quite a bit of depletion, but I think that the graph is misleading...
BEF_data %>% filter(YEAR == 2000) %>%
  filter(SPECIES == "SMB") %>% 
  select(SITE, EFFORT, DSAMP_N, DAY_N) %>% 
  group_by(DAY_N, DSAMP_N, SITE, EFFORT) %>% 
  summarize(total_count = n()) %>% 
  mutate(CPUE = (total_count / EFFORT)*60) %>% 
  ggplot(aes(x = DAY_N, y = CPUE)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  ylab("CPUE Ind/Min") 


## Observation - some sites show depeletion, other do not show depeletion. 
#### Many of those sites look like there were not many fish to depelete anyways? Compared to site 001?
#### Note some sites (like 022 and 023 increase)

BEF_data %>% filter(YEAR == 2000 & SPECIES == "SMB") %>%
  select(SITE, EFFORT, DSAMP_N, DAY_N) %>% 
  group_by(DAY_N, DSAMP_N, SITE, EFFORT) %>% 
  summarize(total_count = n()) %>% 
  mutate(CPUE = (total_count / EFFORT)*60) %>% 
  ggplot(aes(x = DAY_N, y = CPUE)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~SITE) + 
  ylab("CPUE Ind/Min") +
  theme(axis.text.x = element_text(angle = 90))

## It looks like the first day of sampling for sites varied by as much as 20 days
BEF_data %>% filter(YEAR == 2000 & SPECIES == "SMB") %>%
  select(SITE, EFFORT, DSAMP_N, DAY_N) %>% 
  group_by(DAY_N, DSAMP_N, SITE, EFFORT) %>% 
  summarize(total_count = n()) %>% 
  mutate(CPUE = (total_count / EFFORT)*60) %>% 
  ungroup() %>% 
  group_by(SITE) %>% 
  summarize(first_day = first(DAY_N)) %>%
  ggplot(aes(x = order(SITE, first_day), y = first_day)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Site") + ylab("First Day of Sampling")

## Looking at depletion by site            
depletion = BEF_data %>% filter(YEAR == 2000 & SPECIES == "SMB") %>%
  arrange(SITE) %>%
  select(SITE, EFFORT, DSAMP_N, DAY_N) %>% 
  group_by(DAY_N, DSAMP_N, SITE, EFFORT) %>% 
  summarize(total_count = n()) %>% 
  mutate(CPUE = (total_count / EFFORT)*60) %>% 
  group_by(SITE) %>%
  do(day_regression = lm(CPUE ~ DAY_N , data = .))
  
## Explore through the above regressions using....

site_depletion = "005" ## enter the site here that you are interested in. It has to be a character
summary(depletion[[2]][[which(depletion$SITE == site_depletion)]])

## Not many of these are significant... not even site 1. These aren't normal data so this isn't perfect. But I'm not sure there is statistical proof of depletion through time at these sites

## Check below - these should make data frames that list sites with significant depletion


p.value = lapply(depletion$day_regression, function(x) summary(x)) %>%
  lapply(., function(x) p.value = max(x$coefficients[,4])) %>% unlist() %>%
  as.data.frame() %>% 
  rename("p.value" = ".") %>% mutate(depletion$SITE) %>% filter(p.value < .05)
slope = lapply(depletion$day_regression, function(x) summary(x)) %>%
  lapply(., function(x) slope = x$coefficients[,1][2])%>% unlist() %>%
  as.data.frame() %>% 
  rename("slope" = ".") %>% mutate(depletion$SITE) 
left_join(p.value, slope)

## Without filter there are 8 significant depletion, with filter there is only 1 significant depletion


## Conclusion - I'm not sure there is any correct answer here... SMB are the treatment, not a response variable. I'd either pick averaging across all data points for 2000 or filtering within a date range like 130 - 160. 
