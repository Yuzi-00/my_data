library(tidyverse)

data_6P_HE <- read_csv("data/tidydata/data_6P_cal_3nd.csv")
# read in the tidy data for the first six plates (before calculating)


################################################## add amylose content ######################################################


amy <- read_csv("data/tidydata/previous_data/amy_ID_sample.csv")
# read in the tidy data for the amylose content

data_6P_HE_amy <- left_join(data_6P_HE, amy) %>% # combining the data together
  rename(Hydro_extent = HE, Amylose_content = mean_amy) # change the column name

write_csv(data_6P_HE_amy, "data/tidydata/HE_amy_6P.csv")

# need to add the Amy% to C+ and C- (replace the NA) !!!!!!!!!!!!!!!!!!!!!!!


##################################################### add master size #########################################################


master_size <- read_csv("data/tidydata/master_size.csv") # read in the master size dataset

HE_amy_size_6P <- left_join(data_6P_HE_amy, master_size) # combine these dataset together

write_csv(HE_amy_size_6P, "data/tidydata/HE_amy_size_6P.csv")


# to be continued after tidying the other dataset separately first

