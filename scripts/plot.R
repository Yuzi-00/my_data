library(tidyverse)

#######################################################################################################

# ploting using the data_6P_cal dataset 

cal_6P <- read_csv("data/tidydata/data_6P_cal.csv") # read in the dataset with all the calculation

mean_HE_6P <- cal_6P %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE)) # calculating the mean values for plotting

ggplot(data = mean_HE_6P, # plotting by Time and the mean values of HE
       aes(x = Time, 
           y = mean_HE,
           group = Sample,
           color = Sample)) + # too many legends, how to change it ???
  geom_line() +
  geom_point() +
  theme(legend.position = "none") +
  # ??? scale_color_continuous("Price") # need to find the right expression
  # ??? scale_color_manual(values = ) # how to change the color just for one line ????
  facet_wrap(~Sample) # problem: too many samples

control_6P <- cal_6P %>% 
  filter(WellGroupType == "Control-" | WellGroupType == "Control+") %>% 
  group_by(Plate)
# choose the control group and save them to a new dataset

control_6P <- ggplot(data = control_6P, # plotting for the control samples separately
       aes(x = Time, 
           y = HE,
           group = Plate,
           color = Sample)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_line() +
  geom_point() + # problem: if we don't split it into two graphs, the lines are not correctly conneted
  facet_wrap(~Sample)

ggsave("figures/control_6P.png", 
       plot = control_6P, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot

var_control_6P <- cal_6P %>% 
  filter(Sample == "C+" | Sample == "C-") %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE), sd_HE = sd(HE), cov = sd_HE / mean_HE * 100)
# calculating the variation for the control samples

write_csv(var_control_6P, "analysis/var_control_6P.csv")

var_control_6P <- ggplot(data = var_control_6P, # plotting for the control samples using mean and sd
                     aes(x = Time, 
                         y = mean_HE,
                         color = Sample)) + 
  geom_errorbar(aes(ymin=mean_HE - sd_HE, ymax=mean_HE + sd_HE), width=.1) + # adding the sd
  # ??? question here
  geom_line() +
  geom_point() 

ggsave("figures/var_control_6P.png", 
       plot = var_control_6P, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot


#####################################################################################################


# ploting using the data_6P_cal_bis dataset (if we just retain one decimal after the decimal mark)

library(tidyverse)

cal_6P_bis <- read_csv("data/tidydata/data_6P_cal_bis.csv") # read in the dataset with all the calculation

mean_HE_bis <- cal_6P_bis %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE)) # calculating the mean values for plotting

ggplot(data = mean_HE_bis, # plotting by Time and the mean values of HE
       aes(x = Time, 
           y = mean_HE,
           group = Sample)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Sample) # need to resolve the problem (too many samples)

control_6P_bis <- cal_6P_bis %>% 
  filter(WellGroupType == "Control-" | WellGroupType == "Control+") %>% 
  group_by(Plate)
# choose the control group and save them to a new dataset

control_6P_bis <- ggplot(data = control_6P_bis, 
       aes(x = Time, 
           y = HE,
           group = Plate,
           color = Sample)) + 
  geom_line() +
  geom_point() + # problem: if we don't split it into two graphs, the lines are not correctly conneted
  facet_wrap(~Sample) 

ggsave("figures/control_6P_bis.png", 
       plot = control_6P_bis, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot

var_control_6P_bis <- cal_6P_bis %>% 
  filter(Sample == "C+" | Sample == "C-") %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE), sd_HE = sd(HE), cov = sd_HE / mean_HE * 100)
# calculating the variation for the control samples

write_csv(var_control_6P_bis, "analysis/var_control_6P_bis.csv")


#######################################################################################################

# ploting using the data_6P_cal_2nd dataset 

cal_6P_2nd <- read_csv("data/tidydata/data_6P_cal_2nd.csv") # read in the dataset with all the calculation

mean_HE_6P_2nd <- cal_6P_2nd %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE)) # calculating the mean values for plotting

ggplot(data = mean_HE_6P_2nd, # plotting by Time and the mean values of HE
       aes(x = Time, 
           y = mean_HE,
           group = Sample,
           color = Sample)) + # too many legends, how to change it ???
  geom_line() +
  geom_point() +
  theme(legend.position = "none") +
  # ??? scale_color_continuous("Price") # need to find the right expression
  # ??? scale_color_manual(values = ) # how to change the color just for one line ????
  facet_wrap(~Sample) # problem: too many samples

control_6P_2nd <- cal_6P_2nd %>% 
  filter(WellGroupType == "Control-" | WellGroupType == "Control+") %>% 
  group_by(Plate)
# choose the control group and save them to a new dataset

control_6P_2nd <- ggplot(data = control_6P_2nd, # plotting for the control samples separately
                     aes(x = Time, 
                         y = HE,
                         group = Plate,
                         color = Sample)) +
  geom_line() +
  geom_point() + # problem: if we don't split it into two graphs, the lines are not correctly conneted
  facet_wrap(~Sample)

ggsave("figures/control_6P_2nd.png", 
       plot = control_6P_2nd, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot

var_control_6P_2nd <- cal_6P_2nd %>% 
  filter(Sample == "C+" | Sample == "C-") %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE), sd_HE = sd(HE), cov = sd_HE / mean_HE * 100)
# calculating the variation for the control samples

write_csv(var_control_6P_2nd, "analysis/var_control_6P_2nd.csv")

var_control_6P_2nd <- ggplot(data = var_control_6P_2nd, # plotting for the control samples using mean and sd
                         aes(x = Time, 
                             y = mean_HE,
                             color = Sample)) + 
  geom_errorbar(aes(ymin=mean_HE - sd_HE, ymax=mean_HE + sd_HE), width=.1) + # adding the sd
  # ??? question here
  geom_line() +
  geom_point() 

ggsave("figures/var_control_6P_2nd.png", 
       plot = var_control_6P_2nd, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot


