library(tidyverse)

library(readxl)

raw_data <- read_xls("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/MastersizerMagic.xls",
               sheet = "Total data",
               range = "A2:K515")
# read in the raw data

df <- raw_data[-1,] # remove the first empty row

master_size <- df %>% 
  select("Sample Name", "d (0.1)", "d (0.5)", "d (0.9)") %>%  # select just these useful columns
  rename(Sample = "Sample Name", d1 = "d (0.1)", d5 = "d (0.5)", d9 = "d (0.9)") %>% 
# change the column name to be consistant with the other dataset
  group_by(Sample) %>% # group by sample
  summarise(D1 = mean(d1), D5 = mean(d5), D9 = mean(d9)) # calculate the average diameters
# 224 samples in total according to the tibble that has just been created, which is consistant with what we have

HE_amy_6P <- read_csv("data/tidydata/HE_amy_6P.csv")
# read in the tidy dataset with hydrolysis extent and the amylose content

HE_amy_size_6P <- left_join(HE_amy_6P, master_size) 
# combine these two dataset together

write_csv(HE_amy_size_6P, "data/tidydata/HE_amy_size_6P.csv")
