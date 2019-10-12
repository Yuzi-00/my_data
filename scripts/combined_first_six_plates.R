library(tidyverse)

read_csv("data/design/design_with_time.csv")

design_first_six_plate <- design_with_time %>% 
  filter(Plate == 1 | Plate == 2 | Plate == 3 | Plate == 4 | Plate == 5 | Plate == 6)
# just keep the first 6 plates (5184 observations: 8(raw)*12(col)*9(measurements/time points)*6(plates))

filename1 <- "C:/Users/WAN333/Documents/Data school/starch-hydrolysis/data/tidydata/plate0"

filename2 <- '.csv' 

plate <- c('2', '3', '4', '5', '6')

data_plate01 <- read_csv(paste0(filename1,'1',filename2)) 
# read the 1st spreadsheet et and assign it to a new dataframe

for (plt in plate) {
  filename_real <- paste0(filename1, plt, filename2)
  print(filename_real)
  temp_data <- read_csv(filename_real)
  data_first_six_plates <- bind_rows(data_plate01, temp_data)
}

# why just plate01 and 06 ???

data_first_six_plates 






