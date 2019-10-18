library(tidyverse)

cal_6P <- read_csv("data/tidydata/cal_6P.csv") # read in the dataset with all the calculation

cal_6P_filted <- cal_6P %>% 
  filter(Sample != 178, Sample != 101, Sample != 146, Sample != 49) 
# 178, 101, 146 and 49 are empty samples

ggplot(data = cal_6P_filted, 
       aes(x = Time, 
           y = HE,
           group = Sample)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Sample)
# need to resolve the problem (too many samples)

control <- cal_6P %>% 
  filter(WellGroupType == "Control-" | WellGroupType == "Control+") %>% 
  group_by(Plate)
# choose the control group and save them to a new dataset

ggplot(data = control, 
       aes(x = Time, 
           y = HE,
           group = Plate,
           color = Sample)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Sample)

# draft below
