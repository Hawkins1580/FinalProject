# Final Project

# Installing packages
# Comment oackages out after uploading them
install.packages(c("tidyverse", "dplyr", "lubridate", "usmap", "ggplot2", "reshape2", "scales", "ggrepel"))
install.packages("writexl")
install.packages("maps")
install.packages("mapdata")


# Bringing packages into library
library(usmap) 
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)
library(ggrepel)
library(tidyverse)
library(writexl)
library(maps)
library(mapdata)

# Reading in U.S. Sources of Electricity (all in TWh)
US_Sources <- read.csv("/cloud/project/Sources_FinalDataset.csv")


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




# Reading in Electricity Generation by Year
Sources_2001 <- read.csv("/cloud/project/2001_DATA.csv")
Sources_2006 <- read.csv("/cloud/project/2006_DATA.csv")
Sources_2011 <- read.csv("/cloud/project/2011_DATA.csv")
Sources_2016 <- read.csv("/cloud/project/2016_DATA.csv")
Sources_2021 <- read.csv("/cloud/project/2021_DATA.csv")


# Filtering through the variables I want w/AER Codes
  # Nuclear = NUC
  # Wind = WND
  # Hydro = HPS & HYC
  # Other & Biomass = ORW, OTH, WWW, GEO

# 2001 - Filtering for fuel codes that I want
Filter_2001 <- Sources_2001 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "NUC" |
           AER.Fuel.Type.Code == "WND" |
           AER.Fuel.Type.Code == "HPS" |
           AER.Fuel.Type.Code == "HYC" |
           AER.Fuel.Type.Code == "ORW" |
           AER.Fuel.Type.Code == "OTH" |
           AER.Fuel.Type.Code == "WWW" |
           AER.Fuel.Type.Code == "GEO") # filtering for fuel codes
# Converting column to numeric
Filter_2001$Net.Generation.MWh <- as.numeric(as.character(Filter_2001$Net.Generation.MWh))


# 2006 - Filtering for fuel codes that I want
Filter_2006 <- Sources_2006 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "NUC" |
           AER.Fuel.Type.Code == "WND" |
           AER.Fuel.Type.Code == "HPS" |
           AER.Fuel.Type.Code == "HYC" |
           AER.Fuel.Type.Code == "ORW" |
           AER.Fuel.Type.Code == "OTH" |
           AER.Fuel.Type.Code == "WWW" |
           AER.Fuel.Type.Code == "GEO") # filtering for fuel codes
# Converting column to numeric
Filter_2006$Net.Generation.MWh <- as.numeric(as.character(Filter_2006$Net.Generation.MWh))



# 2011 - Filtering for fuel codes that I want
Filter_2011 <- Sources_2011 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "NUC" |
           AER.Fuel.Type.Code == "WND" |
           AER.Fuel.Type.Code == "HPS" |
           AER.Fuel.Type.Code == "HYC" |
           AER.Fuel.Type.Code == "ORW" |
           AER.Fuel.Type.Code == "OTH" |
           AER.Fuel.Type.Code == "WWW" |
           AER.Fuel.Type.Code == "GEO") # filtering for fuel codes 
# Converting column to numeric
Filter_2011$Net.Generation.MWh <- as.numeric(Filter_2011$Net.Generation.MWh)



# 2016 - Filtering for fuel codes that I want
Filter_2016 <- Sources_2016 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "NUC" |
           AER.Fuel.Type.Code == "WND" |
           AER.Fuel.Type.Code == "HPS" |
           AER.Fuel.Type.Code == "HYC" |
           AER.Fuel.Type.Code == "ORW" |
           AER.Fuel.Type.Code == "OTH" |
           AER.Fuel.Type.Code == "WWW" |
           AER.Fuel.Type.Code == "GEO") # filtering for fuel codes
# Converting column to numeric
Filter_2016$Net.Generation.MWh <- as.numeric(as.character(Filter_2016$Net.Generation.MWh))



# 2021 - Filtering for fuel codes that I want
Filter_2021 <- Sources_2021 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "NUC" |
           AER.Fuel.Type.Code == "WND" |
           AER.Fuel.Type.Code == "HPS" |
           AER.Fuel.Type.Code == "HYC" |
           AER.Fuel.Type.Code == "ORW" |
           AER.Fuel.Type.Code == "OTH" |
           AER.Fuel.Type.Code == "WWW" |
           AER.Fuel.Type.Code == "GEO") # filtering for fuel codes 
# Converting column to numeric
Filter_2021$Net.Generation.MWh <- as.numeric(as.character(Filter_2021$Net.Generation.MWh))


##### 2001 - Summing Net Generation by State and Fuel Code 
NetGeneration_2001 <- Filter_2001 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
write_xlsx(NetGeneration_2001, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2001.xlsx")


##### 2006 - Summing Net Generation by State and Fuel Code 
NetGeneration_2006 <- Filter_2006 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
write_xlsx(NetGeneration_2006, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2006.xlsx")


##### 2011 - Summing Net Generation by State and Fuel Code 
NetGeneration_2011 <- Filter_2011 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
write_xlsx(NetGeneration_2011, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2011.xlsx")


##### 2016 - Summing Net Generation by State and Fuel Code 
NetGeneration_2016 <- Filter_2016 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
write_xlsx(NetGeneration_2016, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2016.xlsx")

##### 2021 - Summing Net Generation by State and Fuel Code 
NetGeneration_2021 <- Filter_2021 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
write_xlsx(NetGeneration_2021, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2021.xlsx")


# Reading in Clean Data with Lat / Long Coordinates
FINAL_2001 <- read.csv("/cloud/project/2001.csv")
FINAL_2006 <- read.csv("/cloud/project/2006.csv")
FINAL_2011 <- read.csv("/cloud/project/2011.csv")
FINAL_2016 <- read.csv("/cloud/project/2016.csv")
FINAL_2021 <- read.csv("/cloud/project/2021.csv")




usa <- map_data('usa')
ggplot(data=FINAL_2001, aes(x=Long, y=Lat, group=AER.Fuel.Type.Code)) + 
  geom_polygon(fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3)


# Plotting US Map
plot_usmap(region="state", boundary=FALSE, col="gray", add=TRUE) + 
  labs(title = "Map of United States",
       subtitle = "Renewable Electricity Generation Over Time") + 
  theme(panel.background=element_blank())





