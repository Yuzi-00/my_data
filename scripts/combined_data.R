library(tidyverse)

library(readxl)

data_t0 <- read_excel("data/20190910_DNS plate01-t0min.xlsx",
                      range = "A15:M23") 

data_t20 <- read_excel("data/20190910_DNS plate01-t20min.xlsx",
                      range = "A15:M23") 

data_t60 <- read_excel("data/20190910_DNS plate01-t60min.xlsx",
                      range = "A15:M23") 

data_t120 <- read_excel("data/20190910_DNS plate01-t120min.xlsx",
                      range = "A15:M23") 

data_t180 <- read_excel("data/20190910_DNS plate01-t180min.xlsx",
                      range = "A15:M23") 

data_t240 <- read_excel("data/20190910_DNS plate01-t240min.xlsx",
                      range = "A15:M23") 

data_t360 <- read_excel("data/20190910_DNS plate01-t360min.xlsx",
                      range = "A15:M23") 

data_t1440 <- read_excel("data/20190910_DNS plate01-t1440min.xlsx",
                      range = "A15:M23") 

data_t1800 <- read_excel("data/20190910_DNS plate01-t1800min.xlsx",
                      range = "A15:M23") 

# import the data from excel by choosing just A15 to M23
# save each of them to a new data frame

data_t0 <- mutate(data_t0, time = 0)

data_t20 <-mutate(data_t20, time = 20)

data_t60 <-mutate(data_t60, time = 60)

data_t120 <-mutate(data_t120, time = 120)

data_t180 <-mutate(data_t180, time = 180)

data_t240 <-mutate(data_t240, time = 240)

data_t360 <-mutate(data_t360, time = 360)

data_t1440 <-mutate(data_t1440, time = 1440)

data_t1800 <-mutate(data_t1800, time = 1800)

# adding a column indicating the time point

total_data <- bind_rows(data_t0, data_t20, data_t60, data_t120, data_t180, data_t240, 
                        data_t360,data_t1440, data_t1800) %>% 
  rename(raw = ...1) 

# combining all the data together
# modify the default name for the first col & raw

total_data_transfered <- gather(total_data, col, OD, -raw, -time) %>% 
  mutate(plate = 1)

# transfer the data into two columns 
# adding a column indicating the plate number




