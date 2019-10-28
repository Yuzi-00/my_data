library(tidyverse)

data_6P_HE <- read_csv("data/tidydata/data_6P_without_cal.csv")
# read in the tidy data for the first six plates (before calculating)

amy <- read_csv("data/tidydata/previous_data/amy_ID_sample.csv")
# read in the tidy data for the amylose content

data_6P_HE_amy <- left_join(data_6P_HE, amy)
# combining the data together

# need to check the NA
# to be continued after tidying the other dataset separately first

