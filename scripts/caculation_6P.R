library(tidyverse)

data_6P <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

cal_6P <- data_6P %>% 
  mutate(C_sample = OD_sample / Slope, # calculate the concentration
         C_blk = OD_blk / Slope,
         C_spl_nor = 10 * C_sample / Mass_sample, # calculate the concentration by normalising 
                                                 # the mass at 10 mg
         C_blk_nor = 10 * C_blk / Mass_blk,
         C = C_spl_nor - C_blk_nor,
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
                                  # in theory, the HE should be 10/0.9 = 11 g/L

cal_6P_filted <- cal_6P %>% 
  filter(Mass_sample != 0 & Mass_blk != 0) # remove the empty samples
# 5 samples have been removed here, which is correct. Empty samples: 178, 101, 146, 49, 107
  
write_csv(cal_6P_filted, "data/tidydata/data_6P_cal.csv")

pos_control <- cal_6P_filted %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
                  # use this value to filter the sample which are above it
                # the max extent of hydrolysis of C+ is 91.9%

high_HE <- cal_6P %>% 
  filter(Time == 1800, HE >= 80, Sample != "C+") # select the HE >= 91.9 (positive control) at 1800min (30h)
# the samples have a hydrolysis extent above the positive control are: 208, 62, 212, 177

write_csv(high_HE, "results/high_HE_samples.csv") # save the data of high HE samples
  
sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800)

sample_02 <- cal_6P %>% 
  filter(Sample == 02, Time == 1800)
# check on different samples

neg_control <- cal_6P_filted %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(HE)

low_HE <- cal_6P_filted %>% 
  filter(Time == 1800, HE <= 53.8) # select the HE <= 53.8 (negative control) at 1800min

sample_92 <- cal_6P_filted %>% 
  filter(Sample == 92, Time == 1800) # check on sample 92

######################################################################################################

# if we just retain one decimal after the decimal mark for Slope

library(tidyverse)

data_6P_bis <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

cal_6P_bis <- data_6P_bis %>% 
  mutate(Slope = round(Slope, 1)) %>% # retain just one digit after the mark
  mutate(C_sample = OD_sample / Slope, # calculate the concentration
         C_blk = OD_blk / Slope,
         C_spl_nor = 10 * C_sample / Mass_sample, # calculate the concentration by normalising 
         # the mass at 10 mg
         C_blk_nor = 10 * C_blk / Mass_blk,
         C = C_spl_nor - C_blk_nor,
         HE = C / (10 / 0.9) * 100)

cal_6P_filtered_bis <- cal_6P_bis %>% 
  filter(Mass_sample != 0 & Mass_blk != 0) # remove the empty samples

write_csv(cal_6P_filtered_bis, "data/tidydata/data_6P_cal_bis.csv")

pos_control_bis <- cal_6P_filtered_bis %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
# use this value to filter the sample which are above it
# the max extent of hydrolysis of C+ is 91.9%

high_HE_bis <- cal_6P_filtered_bis %>% 
  filter(Time == 1800, HE >= 77, Sample != "C+") # select the HE >= 77 (positive control) at 1800min (30h)

write_csv(high_HE_bis, "results/high_HE_bis.csv") # save the data of high HE samples

sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800)

sample_02 <- cal_6P %>% 
  filter(Sample == 02, Time == 1800)
# check on different samples 

neg_control_bis <- cal_6P_filtered_bis %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(HE)

low_HE_bis <- cal_6P_filtered_bis %>% 
  filter(Time == 1800, HE <= 49.4) # select the HE <= 49.4 (negative control) at 1800min, no one is below it
# BUT sample 92 is 49.8, very close to the neg control

sample_92_bis <- cal_6P_filtered_bis %>% 
  filter(Sample == 92, Time == 1800) # check on the sample 92


######################################################################################################


# if we use the average of the Slope 

library(tidyverse)

data_6P_2nd <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

cal_6P_2nd <- data_6P_2nd %>% 
  mutate(C_sample = OD_sample / 0.1893, # calculate the concentration, 0.1893 is the average slope
         # 0.1893 was calculated in excel(see slope.xlsx)
         C_blk = OD_blk / 0.1893,
         C_spl_nor = 10 * C_sample / Mass_sample, # calculate the concentration by normalising 
         # the mass at 10 mg
         C_blk_nor = 10 * C_blk / Mass_blk,
         C = C_spl_nor - C_blk_nor,
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
# in theory, the HE should be 10/0.9 = 11 g/L

cal_6P_2nd_filted <- cal_6P_2nd %>% 
  filter(Mass_sample != 0 & Mass_blk != 0) # remove the empty samples
# 4 samples have been removed here: 178(appears twice), 101, 146, 49

write_csv(cal_6P_2nd_filted, "data/tidydata/data_6P_cal_2nd.csv")

pos_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
# use this value to filter the sample which are above it
# the min extent of hydrolysis of C+ is 82.1%

high_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, HE >= 82.1, Sample != "C+") # select the HE >= 81.6 (positive control) at 1800min (30h)
# the samples have a hydrolysis extent above the positive control are: 212, 15, 134, 2, 67, 8

write_csv(high_HE_2nd, "results/high_HE_samples_2nd.csv") # save the data of high HE samples

sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800)

sample_02 <- cal_6P %>% 
  filter(Sample == 02, Time == 1800)
# check on different samples

neg_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(HE)

low_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, HE <= 57.9, Sample != "C-") # select the HE <= 57.9 (negative control) at 1800min
# only sample 92 is below 57.9%

sample_92 <- cal_6P_filted %>% 
  filter(Sample == 92, Time == 1800) # check on sample 92