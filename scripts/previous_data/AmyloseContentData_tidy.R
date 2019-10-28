library(tidyverse)

library(readxl)

amy <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/AmyloseContentData+Lmax_Flour.xlsx")
# read in the amylose content dataset (data from the flour, not the starch)

amy_selected <- amy %>% 
  select(ID, `amylose content estimate`) %>% # selecting just the cols ID and amycontent
  filter(ID != "BLANK", ID != "STDCURVE") # removing the blanks and the standard curves

amy_selected[duplicated(amy_selected),] # checking if there are some duplicated row
# no duplicated row in this dataset

amy_tidy <- amy_selected %>% 
  add_row(ID = "C+", `amylose content estimate` = 0) %>% # add the C+
  add_row(ID = "C-", `amylose content estimate` = 72) %>% # add the C-
  # need to confirm the exact number for HAMY
  group_by(ID) %>% 
  summarise(mean_amy = mean(`amylose content estimate`, na.rm = TRUE)) %>% 
  # each ID has a few amy% 
  # calculating the mean value for each ID
  arrange(mean_amy)
  
design_magic <- read_csv("data/tidydata/previous_data/design_magic_pop.csv")
# read in the design of the magic population (version after tidying)  

amy_ID_sample <- left_join(design_magic, amy_tidy) # combining these two datasets

filter(amy_ID_sample, is.na(mean_amy)) # showing the NA

filter(amy_selected, ID == "cav4080553") # no matching

filter(amy_selected, ID == "cav4081559") # no matching

filter(amy_selected, ID == "cav4081559") # no matching
# as a result, sample 55, 68, 210 hadn't been measured the amy%

write_csv(amy_ID_sample, "data/tidydata/previous_data/amy_ID_sample.csv")


       