library(dplyr)
library(tidyverse)
library(tidyr)
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal")
jml = read.csv("Data/JML.Data.Master.csv") %>%
  select(Species, Group, Date, FISH_N, C, N) %>%
  na.omit() %>%
  #filter(Group == "Fish") %>%
  rename(D13C = C, 
         D15N = N) %>%
  mutate(D13C = as.numeric(D13C), 
         D15N = as.numeric(D15N)) %>%
  mutate(D13C = round(D13C, digits = 2), 
         D15N = round(D15N, digits = 2)) %>%
  mutate(FRAME.JML = "JML") %>%
  na.omit()


colnames(jml)


dat.base = read.csv("Data/SI_MEASUREMENT.csv") %>%
  filter(grepl("JML", COMMENT)) %>%
  filter(!grepl("SIC",COMMENT)) %>%
  #filter(ITEM_N != "") %>%
  #filter(GROUP == "FISH") %>%
  mutate(D13C = round(D13C, digits = 2), 
         D15N = round(D15N, digits = 2))  %>%
  mutate(D13C = as.numeric(D13C), 
         D15N = as.numeric(D15N)) %>%
  mutate(FRAME.datbase = "dat") %>%
  filter(!grepl("FBL", ITEM_N), 
         !grepl("SBL", ITEM_N),
         !grepl("WLL", ITEM_N),
         !grepl("TDL", ITEM_N), 
         !grepl("SDL", ITEM_N),
         !grepl("TBL",ITEM_N), 
         !grepl("TBL", ISO_YSAMP_N))

colnames(dat.base)
  


new = left_join(jml, dat.base %>% select(ISO_YSAMP_N, ISO_FISH_N, ITEM_N, TAXON,CATEGORY, D13C, D15N, FRAME.datbase, COMMENT) , by = c("D13C", "D15N")) %>%
  mutate(issue = case_when(Species != TAXON ~ "issue present", Species == TAXON ~ "entry correct", is.na(TAXON) == T ~ "entry missing"))



colnames(new)


# Dataframe in database format with corrected values
fixed = dat.base %>% left_join(new)

write.csv(fixed, "data_base_errors.csv")

missing = new %>% filter(issue == "entry missing")

write.csv(missing, "missing_data_base.csv")


## I hand-edditted the fixed data frame

edited_fixed = read.csv("data_base_errors.csv") %>%
  filter(grepl("JML", COMMENT)) %>%
  select(-PER_H, -D2H, 
         -CO2_AREA,  -SD_D18O_VSMOW, -CO2_AMP, -PER_N, -N2_AMP, -SAMPLE_TYPE,
         -SD_D13C_VPDB, -SD_D18O_VPDB, -ISO_FISH_N, -H2_AMP, -'D18O_VSMOW', 
         -D18O_VPDB)

d = read.csv("Data/SI_MEASUREMENT.csv") %>% full_join(edited_fixed)

d %>% filter(is.na(issue) ==F)

c = d %>% mutate(TAXON = case_when(issue == "issue pressent" ~ TAXON_corrected,
                               issue != "issue present" ~ TAXON, 
                               is.na(issue) == T ~ TAXON), 
             GROUP = case_when(issue == "issue present"~ GROUP_corrected,
                               issue != "issue present" ~ GROUP, 
                               is.na(issue) == T ~ GROUP),
             CATEGORY = case_when(issue == "issue present"~ CATEGORY_corrected,
                               issue != "issue present" ~ CATEGORY, 
                               is.na(issue) == T ~ CATEGORY),
             ITEM_N = case_when(issue == "issue present"~ ITEM_N_corrected,
                               issue != "issue present" ~ ITEM_N,
                               is.na(issue) == T ~ ITEM_N)) %>%
  mutate(issue = case_when(issue == "issue present" ~ "issue corrected")) %>%
  mutate(issue = replace_na(issue, "no issue"))

 colnames(c) %>% length()

 
 missing_formatted = matrix(nrow = 109, ncol = 33) %>% as.data.frame() %>%
   rename_with(~ colnames(c)) %>%
   mutate(TAXON = missing$Species,
          GROUP = missing$Group, 
          D13C = missing$D13C, 
          D15N = missing$D15N, 
          ISO_YSAMP_N = missing$ISO_YSAMP_N, 
          ISO_FISH_N = missing$FISH_N, 
          ITEM_N = missing$ITEM_N, 
          CATEGORY = missing$CATEGORY, 
          COMMENT = missing$COMMENT, 
          issue = "missing")
colnames(missing_formatted)[34] 

final = rbind(c, missing_formatted)
write.csv(final, "SIA_fixed.csv")
