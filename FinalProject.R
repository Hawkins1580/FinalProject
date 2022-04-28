# Final Project

# Installing packages
# Comment oackages out after uploading them
install.packages(c("dplyr", "lubridate", "usmap", "ggplot2", "reshape2", "scales", "ggrepel"))

# Bringing packages into library
library(usmap) 
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)
library(ggrepel)

# Reading in U.S. Sources of Electricity (all in TWh)
US_Sources <- read.csv("/cloud/project/Sources_FinalDataset.csv")

# Reading in Electricity Generation by Year
Sources_2001 <- read.csv("/cloud/project/2001_DATA.csv")
Sources_2006 <- read.csv("/cloud/project/2006_DATA.csv")
Sources_2011 <- read.csv("/cloud/project/2011_DATA.csv")
Sources_2016 <- read.csv("/cloud/project/2016_DATA.csv")
Sources_2021 <- read.csv("/cloud/project/2021_DATA.csv")


# Renaming column
colnames(US_Sources)
colnames(US_Sources)[1] <- "Year"
colnames(US_Sources)[2] <- "Hydro"
colnames(US_Sources)[3] <- "Other Renewables & Biomass"
colnames(US_Sources)[4] <- "Solar"
colnames(US_Sources)[5] <- "Wind"
colnames(US_Sources)[6] <- "Nuclear"

# Melting all sources into one column 
DataPlot_USEnergy <- melt(US_Sources, id="Year") # sorting each energy sector by year
 # Renaming columns 
colnames(DataPlot_USEnergy)

ElectrictyMix_GRAPH <- ggplot(data = DataPlot_USEnergy, # data for plot
                aes(x = Year,
                    y = value,
                    color = variable))+
  scale_color_manual(values = c("dodgerblue2", # Hydro
                                "tan4", # Other & Biomass
                                "darkorange", # Solar
                                "turquoise", # Wind
                                "darkgoldenrod1"))+ # Nuclear
  geom_line(size = .75)+
  scale_y_continuous(labels = comma)+ 
  labs(x="Year", y="Electricty Production (in TWh)")+ 
  ggtitle("United States Electricity Production by Source")+
  theme_classic()
ElectrictyMix_GRAPH

# Adding labels to each line
# Creating second last year variable to attach label to 
Last_Year <- max(DataPlot_USEnergy$Year[DataPlot_USEnergy$Year == max(DataPlot_USEnergy$Year)])

# Graph with labels on each line
ElectrictyMix_GRAPH + 
  geom_label_repel(data = filter(DataPlot_USEnergy, Year == Last_Year),
                   aes(label = variable),
                   nudge_x = .75,
                   na.rm = TRUE) + 
  theme(legend.position = "none") 




plot_usmap(regions = "states") + 
  labs(title = "Map of United States",
       subtitle = "Renewable Electricity Generation Over Time") + 
  theme(panel.background=element_blank())





