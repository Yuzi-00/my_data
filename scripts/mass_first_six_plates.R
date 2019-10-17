library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/starch_mass/starch plate"
filename2 <- '.xlsx'
# assigning the unchangeable part of the path

filenumber <- c('02', '03', '04', '05', '06') 
# assigning the changeable part of the path, other methods ???

data_plate01 <- read_excel(paste0(filename1,'01',filename2), range = "A1:M9") 
# read the 1st spreadsheet et and assign it to a new dataframe

for (flnum in filenumber) {
  filename_real <- paste0(filename1,flnum,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A1:M9")
  data_plate01 <- bind_rows(data_plate01, temp_data)
}
# creat a loop to import all the spreadsheet
# add column: plate number from the 2nd spreadsheet on 
# bind them by raws

data_plate01 <- rename(data_plate01, raw = ...1) 
# modify the default name for the first col & raw

mass_first_six_plates <- data_plate01 %>%  
  gather(col, mass, -raw) 
# transfer the data into two columns 

mass_first_six_plates <- select(mass_first_six_plates, raw, col, mass) %>% 
  arrange(raw)
# ordering the column names and arrange by the raw

mass_first_six_plates  <- mass_first_six_plates[rep(seq_len(nrow(mass_first_six_plates)), 
                                                    each = 9), ] 
# in order to join this dataset with the other two, each row has to repete 9 times 

write_csv(mass_first_six_plates, "data/tidydata/mass_first_six_plates.csv")




