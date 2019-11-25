library(tidyverse)

library(readxl)

############################################## combine the OD with the design ##################################################################

# read in the design

design <- read_csv("data/design/design_with_time.csv")

filename1 <- "C:/Users/WAN333/Documents/Data school/starch-hydrolysis/data/tidydata/plate"

filename2 <- '.csv' 

plate <- c('02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15') 
# plate <- c(02:15) this is another alternative but have to deal the problem of not showing the 0

# read in the 1st spreadsheet and assign it to a new dataframe

data_15P <- read_csv(paste0(filename1, '01', filename2)) 


for (plt in plate) {
  filename_real <- paste0(filename1, plt, filename2)
  print(filename_real)
  temp_data <- read_csv(filename_real)
  data_15P <- bind_rows(data_15P, temp_data)
}

# below is an ememple of taking one element in the vector(set a value for the 'counter'), 
# and step through the loop, see what the loop is doing (called DEBUG)

# plt <- plate[1]
# filename_real <- paste0(filename1, plt, filename2)
# print(filename_real)
# temp_data <- read_csv(filename_real)
# data_first_six_plates <- bind_rows(data_plate01, temp_data)

# change the column names to be consistent with the design

data_15P <- data_15P %>% 
  rename(Plate = plate, Row = raw, Column = col, Time = time)

# check the missing values

data_15P %>% 
  filter(is.na(OD)) # 446 NAs in total, which is correct

# save the data of these 15 plates

write_csv(data_15P, "data/tidydata/data_15P.csv")

# join the data with the design
# order the columns

joined_15P <- full_join(data_15P, design) %>% 
  select(Plate, Row, ColPair, Column, Sample, WellGroup, WellGroupType, Time, OD)

# save the joined data 

write_csv(joined_15P, "data/tidydata/joined_15P.csv")

############################################ add the mass into the previous joined dataset ########################################################

# read in the mass dataset (tidy version)

mass <- read_csv("data/tidydata/mass_15P.csv") 

# select just the Mass column to be merged in the next step

mass_selected <- select(mass, Mass)

total <-  bind_cols(joined_15P, mass_selected) %>% 
  select(Plate, Row, ColPair, Column, Sample, WellGroup, WellGroupType, Mass, Time, OD) # ordering the columns

# add a column to distnguish the sample and the blank

add_blk <-  total %>% 
  mutate(blank = Column %% 2 == 0) # %% is to check if there is something left over
# for ex, 5 %% 2 should give us 1 
# add a new col to distinguish the sample and the blank 
# so that we can use it to filter our dataset

# extract the samples

Sample <- add_blk %>% 
  filter(blank == FALSE) %>% # just keep the sample columns
  rename(OD_sample = OD, Mass_sample = Mass) %>% # rename the columns 
  select(-blank) # remove that blank column


Blank <- add_blk %>% 
  filter(blank == TRUE) %>% # just keep the blank columns
  rename(OD_blk = OD, Mass_blk = Mass) %>% # rename the columns
  select(Mass_blk, OD_blk) # remove that blank column

# bind the Sample and the Blank together by column

joined_15P_with_mass <- bind_cols(Sample, Blank)

# save the dataset

write_csv(joined_15P_with_mass, "data/tidydata/joined_15P_with_mass.csv")

####################################################################################################################################

slope <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/slope.xlsx"，
                   range = "A2:C56")
# read in the data from the standard curve (maltose), and save it to a new variable slope

total_data_6P_slope <- inner_join(total_data_6P_transformed, slope)
# join the slope with the previous total data using inner_join

write_csv(total_data_6P_slope, "data/tidydata/total_data_6P_slope.csv")
# save the final dataset

design_name <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/Design name starch_flour.xlsx")
# read in the data that contains the id and the sample name

design_renamed <- design_name %>% 
  rename(Sample = "Sample Name", ID = id) # rename the cols 
# to be consistant with the tidy dataset

design_filted <- design_renamed %>% 
  filter(ID != "NA") # remove the NA within the col ID

data_6P_id <- left_join(total_data_6P_slope, design_filted) # join the previous dataset with id

data_6P_final <- select(data_6P_id, Plate, Row, ColPair, Col, Sample, ID, WellGroup, WellGroupType, 
                        Mass_sample, Time, OD_sample, Mass_blk, OD_blk, Slope)
# ordering the cols

write_csv(data_6P_final, "data/tidydata/data_6P_without_cal.csv")

#####################################################################################################

# To be done later, probably in a different script

AmyCon <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/AmyloseContentData_Flour.xlsx")
# read in the dataset of the amylose content

Amy_selected <- AmyCon %>% 
  select(ID, `amylose content estimate`) %>% # select just the id and the AMY cols
  group_by(ID) %>% # group by ID
  summarise(mean_amy = mean(`amylose content estimate`)) # clculate the mean value for each id

with_amy <- left_join(with_id, Amy_selected) # join the amylose content into the previous dataset

write_csv(with_amy, "data/tidydata/total_6P_with_amy.csv")



