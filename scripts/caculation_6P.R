library(tidyverse)

data_6P <- read_csv("data/tidydata/total_data_6P_slope.csv")
# import the final dataset

cal_6P <- data_6P %>% 
  mutate(OD = OD_sample - OD_blk, # caculate the OD (sample-blank)
         c = OD / Slope, # calculate the concentration
         c_nor = 10 * c / Mass, # calculate the concentration by normalising the mass at 10 mg
         HE = c / (10 / 0.9) * 100) # calculate the hydrolysis extent
                                 # in theory, the HE should be 10/0.9 = 11 g/L

write_csv(cal_6P, "data/tidydata/cal_6P.csv")
