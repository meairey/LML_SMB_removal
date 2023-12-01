
### LML Analysis -- Paper 1


### Libraries --------------
library(lattice)
library(MASS)
library(dplyr)
require(pscl) # alternatively can use package ZIM for zero-inflated 
library(lmtest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(ggridges)
library(ecp)
library(gridExtra)
library(ggnewscale)

## Functions source -----------
# This is just setup at the project working directory. Use option in upper right corner of R to get into project directory. For example, on my computer,its stored in my family one-drive
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal")

### Note - this source file will upload the data files. But you need to make sure to correct the source location for those data files for this to work...
source("Function_Source_Files/AFRP_Functions.R")

### Data -------------------

#sample = read.csv("MA2276_Code//Data/FISH_SAMPLE_edited.csv")
sample = read.csv("../AFRP/MA2276_Code/Data/FISH_SAMPLE_2022_editedsitenumbers.csv")




# Boat Electrofishing Data - make sure you specify what parameters you want to query from the database 
BEF_data_unfiltered = filter_data(water = "LML", gear = "BEF",
                                  gear_code = "NAF", 
                                  species = species) %>% 
  filter(MONTH %in% c(5,6), YEAR >= 1998) 


# Removing rare + stocked taxa ------------ 

## Taxa get removed if they are rare or if they are stocked given 

`%nin%` = Negate(`%in%`) # sets up a way to exclude if in a string

rare_threashold = 50 ## change this based on preference. Here it is filtering out rare fish (NRD and BND)

rare = BEF_data_unfiltered %>% ## This defines rare species 
  group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)

stocked = c("LLS", "RT") ## Stocked fish in LML to be excluded from analysis


BEF_data = BEF_data_unfiltered %>%
  filter(SPECIES %nin% c(stocked, rare$SPECIES)) %>% 
  filter(YEAR < 2020) 
#%>%
  #filter(SPECIES != "SMB" | YEAR != 2000 | DAY_N < 160) ## Filter out BEF SMB data from the year 2000 that's later than DAY_N 160. Change this around depending on how you want to filter 2000... 


## LML 
species_names = c("Brown Bulllhead", "Creek Chub", "Common Shiner","Lake Trout","Central Mudminnow", "Pumpkinseed", "Rainbow Smelt", "Round Whitefish", "Smallmouth Bass", "Slimy Sculpin", "Brook Trout", "White Sucker")

codes = data.frame((species_codes = unique(BEF_data$SPECIES))) %>% 
  arrange(species_codes)

codes = data.frame(species_names = species_names, 
                   species = codes$X.species_codes )

## FBL 
#species_names = c("Creek Chub","Lake Trout", "Central Mudminnow", "Smallmouth Bass", "Brook Trout", "White Sucker")
#species_names_fall=   c("Creek Chub", "Central Mudminnow", "Smallmouth Bass", "White Sucker")

# Data setup
CPUE.w.sec = ((CPUE_wide_seconds(BEF_data) %>%
                 unite("Group", c(YEAR, SITE)) %>% 
                 column_to_rownames(., var = "Group") %>% 
                 mutate(sumrow = rowSums(.)) %>%
                 filter(sumrow>0) %>%
                 select(-sumrow)))

CPUE.w.sec.a = ((CPUE_wide_seconds_avg(BEF_data) %>% 
                   column_to_rownames(., var = "YEAR")))

## Working on piecewise regressions -----------------------------------
# CPUE in minutes
#species = colnames(CPUE.w.sec)
v = CPUE.w.sec %>% 
  mutate(y_s = rownames(CPUE.w.sec)) %>%
  pivot_longer(1:length(codes$species),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "site"), sep = "_") %>%
  unite("ID", 
        c(site:Species), 
        sep = "_", 
        remove = F) %>%

  dplyr::select(-site) %>%
  mutate(value = value * 60 * 60 )


# Colorblind pallete
cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73","#CEC6C6", "#0072B2","#E69F00","#F0E442",  "#CC79A7")

graph_list = list() # Create list of graphs for plotting
for(i in 1:length(species)){
  
  # The directory you want to save the file in - if you want to save pdfs
  #pdf(file = paste("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/MA2276_Code/Graphics/LMLP1/",species[i], "_cpue.pdf", sep = ""),  
    #width = 4, # The width of the plot in inches
    #height = 4) # The height of the plot in inches
  
  # Set up data frame
  x = v %>% filter(Species == species[i]) %>%
    mutate(value = as.numeric(value)) %>% 
    dplyr::select(-Species) %>%
    pivot_wider(values_from = value,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-Year) %>%
    as.matrix()
  
  rownames(x) = rownames(CPUE.w.sec.a) # Sequence of years 
  
  # Run changepoint analysis 
  output = e.divisive(x, 
                      R = 499, 
                      alpha = 1, 
                      min.size = 2,
                      sig.lvl = .05)
  
  # Format data
  dat = data.frame(Year = rownames(CPUE.w.sec.a), 
                   color = output$cluster)
  
  v_mod = left_join(v,dat)
  
  
  # If there are multiple breakpoints plot the means of the chunks

  if((2 %in% v_mod$color) == TRUE){
    dat_graph = v_mod %>% filter(Species == species[i])%>%
      mutate(value = round(value))
    
    mean_cpue = dat_graph %>%
      group_by(color) %>% 
      dplyr::summarize(mean = mean(value))
    
    fred = left_join(dat_graph, mean_cpue)
    

    for(z in 1:length(unique(v_mod$color))){
      
      
      
      
      try.yr = dat_graph %>% filter(Year %in% 
                                unique((v_mod %>% 
                                          filter(color == z))$Year)) 
      cluster_means = left_join(fred, mean_cpue)
    }
    
    cluster_line = cluster_means %>% select(Year, color, mean) %>% unique()



    graph_list[[i]] = ggplot() + 
      geom_jitter(data = dat_graph, aes(x = as.numeric(Year), 
                                        y = value,
                                        col = as.character(color)), 
                  alpha = .2, 
                  width = .2, 
                  size = .9)+
      scale_colour_manual(values=cbbPalette) +
      ylab(paste(species_names[i], "Indv / Hour") )+ xlab("Year") + 
      theme(legend.position="none") + 
      geom_vline(xintercept = 2000, linetype = "dashed") +
      xlim(1997, 2022) + 
      theme_minimal()+ 
      theme(legend.position="none") +
      new_scale_color() + 
      scale_colour_manual(values = c("#707173", 
                                              "#7088b8",
                                              "#E69F00", 
                                             "#6fa373"))+
      geom_line(data = cluster_line, aes(x = as.numeric(Year), 
                                         y = mean, 
                                         col = as.character(color),
                                         
                                         ),
                
                lwd = 1) + 
      
      
      theme(text = element_text(size = 9)) + 
      theme(axis.text.x = element_text(angle =90))
    
  } else {
      dat_graph = v_mod %>% filter(Species == species[i])%>%
        mutate(value = round(value)) 
      
      dat_graph.yr = dat_graph %>%
        filter(Year > 2000)
      
      
      dat_graph.pred = dat_graph %>%
        filter(Year > 2000)   %>% 
        mutate(Year = as.numeric(Year)) %>%
        mutate(Year = scale(Year)[,1]) 
      
      
      mean_cpue = dat_graph %>%
        group_by(color) %>% 
        dplyr::summarize(mean = mean(value))
      
      fred = left_join(dat_graph, mean_cpue)

      try(M4 <- zeroinfl(value ~ (Year) | (Year),
                         dist = 'negbin',
                         data = dat_graph.pred))
      
      
      Pred<- predict(M4,newdata = dat_graph.pred, type = "response")
      
      pred_data = cbind(dat_graph.yr, Pred)
      
      graph_list[[i]] = ggplot() + 
        geom_jitter(data = dat_graph,
                    aes(x = as.numeric(Year),y = value),
                    color = "black", 
                    alpha = .2,
                    width = .2,
                    size = .9)+
        geom_line(data = pred_data,
                  aes(x = as.numeric(Year),y = Pred),
                  col = "#726F6F",
                  lwd = 1) + 
        geom_vline(xintercept = 2000,
                   linetype = "dashed") +
        ylab(paste(species_names[i], "CPUE / Hour") ) +
        xlab("Year") + 
        
        scale_colour_manual(values=cbbPalette) +
        #scale_y_continuous(trans = 'log10') + 
        xlim(1997, 2019)  + 
        theme_minimal()+
        theme(legend.position="none") +
        theme(text = element_text(size = 9)) + 
        theme(axis.text.x = element_text(angle =90))
      
    }
    
    print(graph_list[[i]])
    #dev.off()
}
# Grid Arrange Graphic 
do.call("grid.arrange", c(graph_list, ncol=4))


## CPUE Changepoint Summary

cp_data = read.csv("MA2276_Code/Data/LML_CP_data.csv") %>% left_join(codes)
species_data = read.csv("MA2276_Code/Data/LML_SPECIES_DATA.csv")  %>%
  arrange(MEAN_LML_LENGTH) %>% 
  rename(species = SPECIES) 

cp_data = left_join(cp_data, species_data) %>% arrange(MAX_LML_LENGTH)

cp_data %>% ggplot(aes(x = year, 
                       y = species_names,
                       label = direction_shape,
                       color = direction_shape)) +
  geom_text(size = 6, key_glyph = "rect") +
  xlim(2000, 2020) + 
  geom_segment(y = "Lake Trout", yend = "Lake Trout", x = 2001, xend = 2020,
             col = "#00BFC4", 
             lwd = 1.5) + 
  geom_segment(y = "Common Shiner", yend = "Common Shiner", x = 2001, xend = 2020,
             col = "#F8766D", 
             lwd = 1.5) +
  geom_segment(y= "Creek Chub", yend = "Creek Chub", x = 2001, xend = 2020,
             col = "#F8766D",
             lwd = 1.5) + 
  labs(color = "Relative Abundance") + 
  scale_color_manual(labels = c("Decrease", "Increase"), 
                     values = c("#F8766D", "#00BFC4")) + 
  ylab("Species") + 
  xlab("Year") +
  scale_y_discrete(limits = unique(cp_data$species_names)) + 
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=9)) + 
  theme_minimal() + 
  geom_vline(xintercept = 2000, linetype = 2)
## Write a PDF ---------------------
pdf(file = paste("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/MA2276_Code/Graphics/LMLP1/max_length_CPUE_CP.pdf", sep = ""),   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4)
  
dev.off()
   
### Length Changepoint


BEF_data = BEF_data %>% filter(YEAR < 2020)
# Splitting up the change point for length editing it 

graph_list_length = list()
for(i in 1:length(species)){
  
  #pdf(file = paste("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/MA2276_Code/Graphics/LMLP1/",species[i], "_length.pdf", sep = ""),   # The directory you want to save the file in
     # width = 4, # The width of the plot in inches
     # height = 4) # The height of the plot in inches
  year_min = 1997
  x = BEF_data %>% filter(SPECIES == species[i]) %>% 
    filter(YEAR >= year_min) %>%
    select(LENGTH, YEAR, SITE) %>% na.omit() %>% 
    group_by(YEAR, SITE) %>%
    summarize(median_L = mean(LENGTH,na.rm = T)) %>%
    ungroup() %>%
    filter(SITE != "") %>%
    pivot_wider(names_from = SITE, values_from = median_L) 
  
  output = e.divisive(x, R= 499,
                      alpha = 1,
                      min.size = 2, 
                      sig.lvl  = .05)
  
  output_dat = data_frame(YEAR = unique(x$YEAR), 
                          cluster = output$cluster)
  
  dat = BEF_data %>%
    filter(YEAR >= year_min) %>%
    filter(SPECIES == species[i]) %>% 
    left_join(output_dat)
  
  lm_obj = summary(lm(dat$LENGTH ~ dat$YEAR, 
                      na.rm = T))
  
  
  if((2 %in% output_dat$cluster) == TRUE){
    
    
    mean_length = dat %>%
      group_by(cluster) %>% 
      summarize(mean = mean(LENGTH, na.rm = T))
    
    fred = left_join(dat, mean_length)
    
    

    

      
    cluster_means = left_join(fred, mean_length)
      

    
    
    graph_list_length[[i]] = ggplot() +
      theme_minimal() +
      geom_jitter(data = dat, aes(x = as.numeric(YEAR), 
                                  y = LENGTH, 
                                  col = as.character(cluster),
                                  alpha = .2,
                                  width = .2,
                                  ), size = .9) +
      labs(col = paste(species[i])) + 
      ylab("Length") +
      ylab(paste(species_names[i], "TL (mm)")) + 
      theme(text = element_text(size = 9)) + 
      theme(legend.position="none") +
      xlab("Year") +
      xlim(1997, 2021) + 
      scale_colour_manual(values=cbbPalette) + 
      geom_vline(xintercept = 2003, linetype = "dashed") + 
      new_scale_color() + 
      scale_colour_manual(values = c("#707173", 
                                              "#7088b8",
                                              "#E69F00", 
                                              "#6fa373"))+
      geom_line(data = cluster_means, aes(x = YEAR, 
                                          y = mean,
                                          col = as.character(cluster)),
                    size = 1)
    
    
    
    
    
  } else {
    
    data_graph = dat %>%
      filter(SPECIES == species[i])
    

  graph_list_length[[i]] = ggplot(data = data_graph, 
                                  aes(x = YEAR, y = LENGTH)) +
    theme_minimal() +
    geom_jitter(aes(col = 'red',
                    alpha = .2),
                width = .2, 
                size = .9) +
    labs(col = paste(species[i])) + 
    ylab("Length") +
    geom_smooth(method = "lm", color = "#726F6F", size = 1, se = F) + 
    ylab(paste(species_names[i], "TL (mm)")) + 
    theme(text = element_text(size = 9)) + 
    theme(legend.position="none",
          #axis.title.x = element_blank(),
    ) + 
    xlim(1997, 2021) + 
    xlab("Year") +
    scale_colour_manual(values=cbbPalette)+ 
    geom_vline(xintercept = 2003, linetype = 2)
  
  
  
  
  }
  
  #print(h)
  
  #dev.off()
  
}
#install.packages("ggnewscale")
do.call("grid.arrange", c(graph_list_length, ncol=4))



species_length_changepoint = read.csv("MA2276_Code/Data/changepoint_length.csv") %>% left_join(codes)

cp_data = left_join(species_length_changepoint, species_data) %>% arrange(MAX_LML_LENGTH)


# In that data file i added in - signs next to species that dont actually have a trend to get rid of an extra color on the graphs... seemms to have worked but is not elegant 

species_length_changepoint %>% ggplot(aes(x = year, 
                                          y = species_names,
                                          label = direction,
                                          color = direction)) + 
  geom_text(size = 6,key_glyph = "rect") +
  geom_vline(xintercept = 2000, linetype = 2) +
  xlim(2000, 2020) + 
  theme_minimal() +
  #theme(legend.position = 'none') + 
  geom_segment(y = "Common Shiner", yend = "Common Shiner", x = 2001, xend = 2020,
             col = "#00BFC4", 
             lwd = 1.5) + 
  geom_hline(yintercept = "Brown Bullhead",
             col = "#F8766D", 
             lwd = 1.5) +
  geom_segment(x = 2004, xend = 2020, 
               y = "Central Mudminnow", yend = "Central Mudminnow",
               lwd = 1.5, col = "#F8766D") +

  geom_segment(y = "White Sucker", yend = "White Sucker", x = 2001, xend = 2020,
             col = "#00BFC4", 
             lwd = 1.5) +
  labs(color = "Total Length (mm)", size = 10) + 
  scale_color_manual(labels = c("Decrease", "Increase"), values = c("#F8766D", "#00BFC4")) + 
  ylab("Species") + 
  xlab("Year") +
  scale_y_discrete(limits = unique(cp_data$species_names))+
  theme(legend.title=element_text(size=10),
                            legend.text=element_text(size=9))



pdf(file = paste("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/MA2276_Code/Graphics/LMLP1/max_length_LENGTH_CP.pdf", sep = ""),   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4)

dev.off()

##------------------------------


cat = BEF_data %>% filter(SPECIES == "WS") %>% select(LENGTH, YEAR) 
summary(lm(cat$LENGTH ~ cat$YEAR))



### ------------box plots final ----------------------------

CPUE.w.sec %>% rownames_to_column(var = "year_site") %>%
  separate(year_site, into = c("year", "site")) %>% 
  group_by(year) %>%
  filter(year %in% c(2000,2001, 2019)) %>% 
  pivot_longer(3:14, values_to = "CPUE", names_to = "species") %>% 
  left_join(codes) %>%
  mutate(CPUE = CPUE * 60 * 60) %>%
  ggplot(aes(x = year, y = CPUE)) +
  theme_bw() +
  geom_boxplot()+ 
  #scale_y_continuous(
   # labels = scales::number_format(accuracy = 0.01)) +
  xlab("") + ylab("CPUE (Individuals / Hour)") +
  facet_wrap(~species_names, scales = "free_y")


BEF_data %>% filter(SPECIES %nin% c("MM")) %>%
  rename(species = SPECIES) %>%
  left_join(codes) %>%
  group_by(species) %>% filter(YEAR %in% c(2000, 2001, 2019)) %>%
  
  ggplot(aes(x = as.character(YEAR), y = LENGTH)) +
  theme_bw() + geom_boxplot() + 
  facet_wrap(~species_names, scales = "free_y") + xlab("") + ylab("Total Length (mm)")



### ------------ Wilcoxon Tests ----------------------------

c.data = v %>% select(Year, Species,ID,  value) %>% rename(year = Year) %>% separate(ID, into = c("site", "sp.del")) %>% select(-sp.del) %>% rename(species = Species) %>% rename(CPUE = value)




wilcox_list = list()
for(i in 1:length(species)){
  
    dat = c.data %>% 
        filter(species == species[i])
    # data frames
    data.2000 = dat %>% filter(year == 2000)
    data.2001 = dat %>% filter(year == 2001)
    data.2019 = dat %>% filter(year == 2019)
  
    # Wilcox tests 
    a = wilcox.test((data.2000 %>% filter(species==species[i]))$CPUE, (data.2001 %>% filter(species==species[i]))$CPUE, exact = F)
    b = wilcox.test((data.2000 %>% filter(species==species[i]))$CPUE, (data.2019 %>% filter(species==species[i]))$CPUE, exact = F)
    c = wilcox.test((data.2001 %>% filter(species==species[i]))$CPUE, (data.2019 %>% filter(species==species[i]))$CPUE, exact = F)
    
    
    # Build dataframe 
    
    wilcox_list[[i]] = data.frame(name = rep(species[i], 3), year = c(2000, 2001, 2019), p.value = c(a$p.value, b$p.value, c$p.value), W_statistic = c(a$statistic, b$statistic, c$statistic))
    
    
}


wilcox_list
wilcox_table = rbind(wilcox_list[[1]], 
      wilcox_list[[2]],
      wilcox_list[[3]],
      wilcox_list[[4]],
      wilcox_list[[5]],
      wilcox_list[[6]],
      wilcox_list[[7]],
      wilcox_list[[8]],
      wilcox_list[[9]],
      wilcox_list[[10]],
      wilcox_list[[11]],
      wilcox_list[[12]]
      ) %>% as.data.frame() %>% mutate(p.value = round(p.value, digits = 4)) %>% 
  mutate(p.value = replace(p.value, p.value < 0.0001, "<0.0001")) 

write.csv(wilcox_table, file = "wilcox_table_CPUE.csv")

## ------------------------- Wilcox Length ---------------------



wilcox_list_length = list()
#species_no.mm = species[c(-1, -5)]
species_no.mm = species
for(i in 1:length(species_no.mm)){
  dat = BEF_data %>% filter(SPECIES == species_no.mm[i]) 
  
  data.2000 = dat %>% filter(YEAR == 2000)
  
  data.2001 = dat %>% filter(YEAR == 2001)
  data.2019 = dat %>% filter(YEAR == 2019)
  
  if(length(data.2000[,1]) > 4){
    a = wilcox.test(data.2000$LENGTH, data.2001$LENGTH, exact = F)
  } else {
    a = data.frame(p.value = "NA", statistic = "NA")
  }
  
  if(length(data.2000[,1]) > 4 & length(data.2019[,1]) > 4){
    b = wilcox.test(data.2000$LENGTH, data.2019$LENGTH, exact = F)
  } else {
    b = data.frame(p.value = "NA", statistic = "NA")
  }
  
  if(length(data.2019[,1]) > 5 & length(data.2001[,1]) >5 & is.na(unique(data.2001$LENGTH)) == FALSE){
    c = wilcox.test(data.2001$LENGTH, data.2019$LENGTH, exact = F)
  } else {
    c = data.frame(p.value = "NA", statistic = "NA")
  }
  
  wilcox_list_length[[i]] = data.frame(name = rep(species_no.mm[i], 3),
                                       year = c(2000, 2001, 2019), 
                                       p.value = c(a$p.value, b$p.value, c$p.value), 
                                       W_statistic = c(a$statistic, b$statistic, c$statistic))

}

wilcox_list_length

wilcox_table = rbind(wilcox_list_length[[1]], 
                     wilcox_list_length[[2]],
                     wilcox_list_length[[3]],
                     wilcox_list_length[[4]],
                     wilcox_list_length[[5]],
                     wilcox_list_length[[6]],
                     wilcox_list_length[[7]],
                     wilcox_list_length[[8]],
                     wilcox_list_length[[9]],
                     wilcox_list_length[[10]]) 
  


write.csv(wilcox_table, file = "wilcox_table_length.csv")


### end ------------------------------------------------





BEF_data %>% group_by(SPECIES, YEAR) %>% summarize(max = max(LENGTH, na.rm=T)) %>%
  ggplot(aes(x = YEAR, y = max, col = SPECIES)) + geom_point() + geom_smooth(method= "lm", se = F)
  

BEF_data %>% group_by(SPECIES, YEAR) %>% summarize(min = min(LENGTH, na.rm=T)) %>%
  ggplot(aes(x = YEAR, y = min, col = SPECIES)) + geom_point() + geom_smooth(method= "lm", se = F)


BEF_data %>% group_by(SPECIES) %>% summarize(max = max(LENGTH, na.rm= T))
BEF_data %>% group_by(SPECIES) %>% summarize(min = min(LENGTH, na.rm= T))





##### ------- trying a new metric for length --------------

min_max_dat = BEF_data %>% group_by(SPECIES) %>% summarize(max = max(LENGTH, na.rm= T), min = min(LENGTH, na.rm= T))

ratio_dat = BEF_data  %>% group_by(SPECIES,YEAR) %>% summarize(year_min =min(LENGTH, na.rm = T), year_max = max(LENGTH, na.rm =T)) %>% left_join(min_max_dat) %>% mutate(min_ratio = year_min / min, max_ratio = year_max/max) %>% mutate(hmm = min_ratio/max_ratio)

ratio_dat %>% ggplot(aes(min_ratio, max_ratio)) + geom_point() + facet_wrap(~SPECIES)
  
ratio_dat %>% ggplot(aes(x = YEAR, y = hmm, col = SPECIES)) + geom_point() + geom_smooth(method = "lm",se = F) + facet_wrap(~SPECIES)


for(i in 1:length(species)){
 
  boop = ratio_dat %>% filter(SPECIES == species[i], min_ratio < 100000, max_ratio < 10000) %>% 
    ungroup() %>% 
    select(YEAR, min_ratio, max_ratio) 

  
  output = e.divisive(boop, 
                      R = 499, 
                      alpha = 1, 
                      min.size = 2,
                      sig.lvl = .05)

  boop =  boop %>% mutate(cluster = output$cluster)
  
  p = BEF_data %>%
    filter(SPECIES == species[i]) %>% 
    left_join(boop) %>% 
    ggplot(aes(x = YEAR, y = LENGTH, col = as.factor(cluster))) +
    geom_jitter(alpha = .6, width = .02) +
    labs(col = paste(species[i]))
  
  
  print(p)
}





data.2000




data.2000 %>% pivot_wider(names_from = Species, values_from = value)



boop %>% ggplot(aes(x = YEAR, y =max_ratio)) + geom_point(col = output$cluster) + geom_smooth(method = "lm")

 ## BB
wilcox.test(data.2000$BB, data.2001$BB, exact = F)
wilcox.test(data.2000$BB, data.2019$BB, exact = F)
wilcox.test(data.2001$BB, data.2019$BB, exact = F)

## CC 
wilcox.test(data.2000$CC, data.2001$CC, exact = F)
wilcox.test(data.2000$CC, data.2019$CC, exact = F)
wilcox.test(data.2001$CC, data.2019$CC, exact = F)

## LT 
wilcox.test(data.2000$LT, data.2001$LT, exact = F)
ks.test(data.2000$LT, data.2001$LT, exact = F)
wilcox.test(data.2000$LT, data.2019$LT, exact = F)
wilcox.test(data.2001$LT, data.2019$LT, exact = F)

## CS

wilcox.test(data.2000$CS, data.2001$CS, exact = F)
wilcox.test(data.2000$CS, data.2019$CS, exact = F)
wilcox.test(data.2001$CS, data.2019$CS, exact = F)

#WS
wilcox.test(data.2000$WS, data.2001$WS, exact = F)
wilcox.test(data.2000$WS, data.2019$WS, exact = F)
wilcox.test(data.2001$WS, data.2019$WS, exact = F)

# RS
wilcox.test(data.2000$RS, data.2001$RS, exact = F)
wilcox.test(data.2000$RS, data.2019$RS, exact = F)
wilcox.test(data.2001$RS, data.2019$RS, exact = F)

#ST
wilcox.test(data.2000$ST, data.2001$ST, exact = F)
wilcox.test(data.2000$ST, data.2019$ST, exact = F)
wilcox.test(data.2001$ST, data.2019$ST, exact = F)

## MM
wilcox.test(data.2000$MM, data.2001$MM, exact = F)
wilcox.test(data.2000$MM, data.2019$MM, exact = F)
wilcox.test(data.2001$MM, data.2019$MM, exact = F)

## RWF
wilcox.test(data.2000$RWF, data.2001$RWF, exact = F)
wilcox.test(data.2000$RWF, data.2019$RWF, exact = F)
wilcox.test(data.2001$RWF, data.2019$RWF, exact = F)

## PS
wilcox.test(data.2000$PS, data.2001$PS, exact = F)
wilcox.test(data.2000$PS, data.2019$PS, exact = F)
wilcox.test(data.2001$RWF, data.2019$RWF, exact = F)


# SS
data.2000.SS = v %>% filter(species == "SS" & Year == 2000)
data.2001.SS = v %>% filter(species == "SS"& Year == 2001)
data.2019.SS = v %>% filter(species == "SS"& Year == 2019)
wilcox.test(data.2000.SS$value, data.2001.SS$value, exact = F)
wilcox.test(data.2001.SS$value, data.2019.SS$value, exact = F)
wilcox.test(data.2000.SS$value, data.2019.SS$value, exact = F)

# SS
wilcox.test(data.2000$SMB, data.2001$SMB, exact = F)
wilcox.test(data.2000$SMB, data.2019$SMB, exact = F)
wilcox.test(data.2001$SMB, data.2019$SMB, exact = F)



#### Trying Tommy's heatmap graphs 


v %>% separate(ID, into = c("site", "Species")) %>% mutate(binned = .bincode(value, bins)) %>% 
  group_by(Year, Species, binned) %>% summarize(sum = n())%>% 
  filter(Species == "SMB") %>% 
  ggplot(aes(x = Year, y = binned, fill = sum)) + geom_tile() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(fill = "SMB") + 
  ylab("Increasing relative abundance (binned)")






v %>% separate(ID, into = c("site", "Species")) %>% filter(Year == 2021) %>% mutate(site= as.numeric(site)) %>% filter(site %in% c(14,15,16,18,19,20,21,22)) %>%
  mutate(site = as.factor(site)) %>%
  ggplot(aes(x = site, y = value, fill = Species)) + geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +ylab("CPUE ind/hour")


### Extra Code
BEF_data_2000 = BEF_data_unfiltered %>%
  filter(SPECIES %nin% remove) %>% 
  filter(YEAR %in% c(2000)) %>% 
  filter(SPECIES == "SMB") %>%
  group_by(YEAR, SITE, DAY_N, DSAMP_N, EFFORT) %>% 
  summarize(CPUE = n()) %>%
  mutate(CPUE = CPUE / EFFORT) %>% 
  ungroup() %>%
  group_by(YEAR, SITE) %>%
  summarize(first = first(CPUE)) %>% 
  mutate(Species = "SMB") %>% 
  mutate(ID = paste(SITE, Species, sep = "_"))
