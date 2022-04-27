# Final Project

# Installing packages
# Comment oackages out after uploading them
install.packages(c("dplyr", "lubridate", "usmap", "ggplot2"))

# Bringing packages into library
library(usmap) 
library(ggplot2)
library(dplyr)
library(lubridate)


plot_usmap(regions = "states") + 
  labs(title = "Map of United States",
       subtitle = "Renewable Electricity Generation Over Time") + 
  theme(panel.background=element_blank())





