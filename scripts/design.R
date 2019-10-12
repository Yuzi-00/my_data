library(tidyverse)

library(readxl)

design <- read_xlsx("data/design/Design_Starch-digestability-assay_2009.xlsx")

time <- c(0, 20, 60, 120, 180, 240, 360, 1440, 1800)
# creat a vector

design_with_time <- design[rep(seq_len(nrow(design)), each = 9), ] %>% 
# each raw repeats 9 times
  mutate(Time = rep(time, times = 1440)) # add a column called "Time" and repeat 1440 times

write_csv(design_with_time, "data/design/design_with_time.cvs")




  

