library(tidyverse)

install.packages("naniar")

library(naniar)

# read in the calculated dataset

data_15P <- read_csv("data/tidydata/data_15P_cal.csv")

# add a new column that descripe the status of each samples 

data_status <- data_15P %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

# plot with the 3 replicates separately

ggplot(data = data_status, 
       aes(x = Time, 
           y = HE,
           group = Sample,
           color = status)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 2, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  scale_color_discrete(labels = c("Negtive control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")

# noted that there's an outlier (very sure that it's contaminated) at 20 min, need to change this value into NA

# select just the time at 20minutes

data_t20min <- data_status %>% 
  filter(Time == 20)

# find the outlier

which.max(data_t20min$HE) # it's the number 18th

data_t20min$Sample[18] # it's the sample 208

# change the value into NA, HOW ????????????????????????????????????????????????

## data_15P_remove_outlier <- data_15P %>% 
## mutate(HE = na_if(HE, "92") # if the value in column OD is 92.81697, replace them by NA 

# remove the entire row of that outlier for now

data_15P_outlier_deleted <- data_15P %>% 
  filter(!(Sample == 208 & HE > 92))

# save the dataset without the outlier (208 at t20min)

write_csv(data_15P_outlier_deleted, "data/tidydata/data_15P_outlier_deleted.csv")

# replot without outlier

data_status <- data_15P_outlier_deleted %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

ggplot(data = data_status, 
       aes(x = Time, 
           y = HE,
           group = Sample,
           color = status)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 2, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  scale_color_discrete(labels = c("Negtive control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")


##########################

df %>% 
  filter(Time == 240, cov >= 10) # check the big variance samples at t = 360min
# 15 samples

df %>% 
  filter(Time == 360, cov >= 10) # check the big variance samples at t = 360min
# 18 samples

df %>% 
  filter(Time == 1440, cov >= 10) # check the big variance samples at t = 1440min
# 4 samples : 122, 139, 220, 92


df %>% 
  filter(Time ==1800, cov >= 10) # check the big variance samples at t = 1800min
# 5 samples : 134, 139, 83, 92, 96

# Apparently sample 92 and 139 have a quite big variance both at 1440 and 1800min 


sample_6P_3nd <- ggplot(data = df, # plotting by Time and the mean values of HE
                        aes(x = Time, 
                            y = mean_HE,
                            group = Sample,
                            color = Sample %in% c("C+", "C-"))) + 
  geom_line() +
  geom_point() +
  theme(legend.position = "none")  # remove the legends cuz there are too many
# ??? scale_color_continuous("Price") # need to find the right expression
# ??? scale_color_manual(values = ) # how to change the color just for one line ???


ggsave("figures/sample_6P_3nd.png", 
       plot = sample_6P_3nd, 
       width = 10, 
       height = 8, 
       units = "cm") # save the plot 
# need to change the thickness of the line !!!!!!!!!!!!!!!





