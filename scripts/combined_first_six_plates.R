library(tidyverse)

library(readxl)

design <- read_csv("data/design/design_with_time.csv")

design_first_six_plate <- design %>% 
  filter(Plate %in% 1:6) # logical vector operator: which element of the Plate are 1 to 6
# just keep the first 6 plates 
# (5184 observations: 8(raw)*12(col)*9(measurements/time points)*6(plates))

# design %>% filter(Plate %in% 1:6)

filename1 <- "C:/Users/WAN333/Documents/Data school/starch-hydrolysis/data/tidydata/plate"

filename2 <- '.csv' 

plate <- c('02', '03', '04', '05', '06') 

# alternative: plate <- c(02:06)

data_first_six_plates <- read_csv(paste0(filename1, '01', filename2)) 
# read the 1st spreadsheet et and assign it to a new dataframe

for (plt in plate) {
  filename_real <- paste0(filename1, plt, filename2)
  print(filename_real)
  temp_data <- read_csv(filename_real)
  data_first_six_plates <- bind_rows(data_first_six_plates, temp_data)
}

# below is an ememple of taking one element in the vector(set a value for the 'counter'), 
# and step through the loop, see what the loop is doing (called DEBUG)

# plt <- plate[1]
# filename_real <- paste0(filename1, plt, filename2)
# print(filename_real)
# temp_data <- read_csv(filename_real)
# data_first_six_plates <- bind_rows(data_plate01, temp_data)

data_first_six_plates <- data_first_six_plates %>% 
  rename(Plate = plate, Row = raw, Col = col, Time = time)

design_first_six_plate <- design_first_six_plate %>% 
  rename(Col = Column)

write_csv(data_first_six_plates, "data/tidydata/data_first_six_plates.csv")
# save the data of the first six plates

write_csv(design_first_six_plate, "data/tidydata/design_first_six_plates.csv")
# save the design of the first six plates

joined_data_design_first_six_plates <- full_join(data_first_six_plates, design_first_six_plate) %>% 
  select(Plate, Row, ColPair, Col, Sample, WellGroup, WellGroupType, Time, OD)
# join the data with the design for the first six plates
# order the cols

write_csv(joined_data_design_first_six_plates, "data/tidydata/joined_data_design_first_six_plates.csv")

mass_first_six_plates <- read_csv("data/tidydata/mass_first_six_plates.csv")

mass <- select(mass_first_six_plates, mass)

total_data_first_six_plates <-  bind_cols(joined_data_design_first_six_plates, mass) %>% 
  select(Plate, Row, ColPair, Col, Sample, WellGroup, WellGroupType, mass, Time, OD) %>% 
  rename(Mass = mass)
# rename all the cols to be the same pattern as the design

add_blk <-  total_data_first_six_plates %>% 
  mutate(blank = Col %% 2 == 0) # %% is to check if there is something left over
# for ex, 5 %% 2 should give us 1 
# add a new col to distinguish the sample and the blank 
# so that we can use it to filter our dataset

Sample <- add_blk %>% 
  filter(blank == FALSE) %>% # just keep the sample cols
  rename(OD_sample = OD) %>% # rename the cols 
  select(-blank) # remove that blank col


Blank <- add_blk %>% 
  filter(blank == TRUE) %>% # just keep the blank cols
  rename(OD_blk = OD) %>% # rename the cols
  select(OD_blk) # remove that blank col

total_data_6P <- bind_cols(Sample, Blank)
# bind the Sample and the Blank together by cols

slope <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/slope.xlsx"，
                   range = "A2:C56")
# read in the data from the standard curve (maltose), and save it to a new variable slope

total_data_6P_slope <- inner_join(total_data_6P, slope)
# join the slope with the previous total data using inner_join

write_csv(total_data_6P, "data/tidydata/total_data_6P_slope.csv")
# save the final dataset