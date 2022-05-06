# Final Project

# Installing packages
# Comment oackages out after uploading them
install.packages(c("tidyverse", "dplyr", "lubridate", "usmap", "ggplot2", "reshape2", "scales", "ggrepel"))
install.packages("writexl")
install.packages("maps")
install.packages("mapdata")
install.packages("sf")
install.packages("tmap")


# Bringing packages into library
library(usmap) 
library(reshape2)
library(scales)
library(ggrepel)
library(tidyverse)
library(writexl)
library(maps)
library(mapdata)
library(sf)
library(tmap)


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
  ggtitle("United States Electricity Production by Low-Carbon Source")+
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

# 2001 - Filtering for fuel code that I want to analyze
Filter_2001 <- Sources_2001 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "WND" ) # filtering for fuel wind
# Converting column to numeric
Filter_2001$Net.Generation.MWh <- as.numeric(as.character(Filter_2001$Net.Generation.MWh))


# 2006 - Filtering for fuel code that I want
Filter_2006 <- Sources_2006 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "WND") # filtering for wind
# Converting column to numeric
Filter_2006$Net.Generation.MWh <- as.numeric(as.character(Filter_2006$Net.Generation.MWh))


# 2011 - Filtering for fuel code that I want
Filter_2011 <- Sources_2011 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "WND") # filtering for wind
# Converting column to numeric
Filter_2011$Net.Generation.MWh <- as.numeric(Filter_2011$Net.Generation.MWh)


# 2016 - Filtering for fuel code that I want
Filter_2016 <- Sources_2016 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "WND") # filtering for wind
# Converting column to numeric
Filter_2016$Net.Generation.MWh <- as.numeric(as.character(Filter_2016$Net.Generation.MWh))


# 2021 - Filtering for fuel code that I want
Filter_2021 <- Sources_2021 %>% # data frame with pipe
  group_by(State) %>% # group data frame by state
  filter(AER.Fuel.Type.Code == "WND") # filtering for wind 
# Converting column to numeric
Filter_2021$Net.Generation.MWh <- as.numeric(as.character(Filter_2021$Net.Generation.MWh))


##### 2001 - Summing Net Generation by State and Fuel Code 
NetGeneration_2001 <- Filter_2001 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
# ALSO MAKING SURE EVERY STAT IS LISTED - IMPORTANT FOR MAPPING
write_xlsx(NetGeneration_2001, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2001.xlsx")


##### 2006 - Summing Net Generation by State and Fuel Code 
NetGeneration_2006 <- Filter_2006 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
# ALSO MAKING SURE EVERY STAT IS LISTED - IMPORTANT FOR MAPPING
write_xlsx(NetGeneration_2006, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2006.xlsx")


##### 2011 - Summing Net Generation by State and Fuel Code 
NetGeneration_2011 <- Filter_2011 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
# ALSO MAKING SURE EVERY STAT IS LISTED - IMPORTANT FOR MAPPING
write_xlsx(NetGeneration_2011, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2011.xlsx")


##### 2016 - Summing Net Generation by State and Fuel Code 
NetGeneration_2016 <- Filter_2016 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
# ALSO MAKING SURE EVERY STAT IS LISTED - IMPORTANT FOR MAPPING
write_xlsx(NetGeneration_2016, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2016.xlsx")

##### 2021 - Summing Net Generation by State and Fuel Code 
NetGeneration_2021 <- Filter_2021 %>% 
  group_by(Year, State, AER.Fuel.Type.Code) %>% 
  summarise(Net.Generation.MWh = sum(Net.Generation.MWh))
# Exporting to excel so I can add lat / long coordinates
# ALSO MAKING SURE EVERY STAT IS LISTED - IMPORTANT FOR MAPPING
write_xlsx(NetGeneration_2021, "\\Client\\C$\\Users\\nickhawkins\\Desktop\\2021.xlsx")


# Reading in Clean Data with Lat / Long Coordinates
FINAL_2001 <- read.csv("/cloud/project/2001.csv")
FINAL_2006 <- read.csv("/cloud/project/2006.csv")
FINAL_2011 <- read.csv("/cloud/project/2011.csv")
FINAL_2016 <- read.csv("/cloud/project/2016.csv")
FINAL_2021 <- read.csv("/cloud/project/2021.csv")

stateID <- read_sf("/cloud/project/cb_2018_us_state_500k.shp")

# Combining all data into one dataframe
ALL_Data <- list(FINAL_2001, FINAL_2006, FINAL_2011, FINAL_2016, FINAL_2021)
# Using rbind 
ALL_DataFrames <- do.call("rbind", ALL_Data)
# joining dataframes to get state ID to match up
ID_All <- left_join(stateID, ALL_DataFrames, by=c("STUSPS"="State"))

# Renaming Net.Generation.MWh column
colnames(ID_All)
colnames(ID_All)[13] <- "MWh"


# Wind 2001 - Creating dataframe for wind and omitting NAs
WND_2001 <- ID_All[ID_All$AER.Fuel.Type.Code=="WND" & ID_All$Year==2001,]
New_WND_2001 <- na.omit(WND_2001)

# Wind 2006 - Creating dataframe for wind and omitting NAs
WND_2006 <- ID_All[ID_All$AER.Fuel.Type.Code=="WND" & ID_All$Year==2006,]
New_WND_2006 <- na.omit(WND_2006)

# Wind 2011 - Creating dataframe for wind and omitting NAs
WND_2011 <- ID_All[ID_All$AER.Fuel.Type.Code=="WND" & ID_All$Year==2011,]
New_WND_2011 <- na.omit(WND_2011)

# Wind 2016 - Creating dataframe for wind and omitting NAs
WND_2016 <- ID_All[ID_All$AER.Fuel.Type.Code=="WND" & ID_All$Year==2016,]
New_WND_2016 <- na.omit(WND_2016)

# Wind 2021 - Creating dataframe for wind and omitting NAs
WND_2021 <- ID_All[ID_All$AER.Fuel.Type.Code=="WND" & ID_All$Year==2021,]
New_WND_2021 <- na.omit(WND_2021)


# Making Figure #2 - Wind Plots Over Time

# Plot 2001 Wind
tmap_mode("plot")
tm_shape(New_WND_2001)+
  tm_fill("MWh",
          title="Net Generation (in MWh)",
          palette = "YlGn",
          n=4,style="jenks")+ # creates natural breaks
  tm_borders(col="black", lwd=1)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            main.title = "2001 Electricity Generation from Wind Energy", 
            title.size = 1, main.title.position="left")

# Plot 2006 Wind
tm_shape(New_WND_2006)+
  tm_fill("MWh",
          title="Net Generation (in MWh)",
          palette = "YlGn",
          n=4,style="jenks")+ # creates natural breaks
  tm_borders(col="black", lwd=1)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            main.title = "2006 Electricity Generation from Wind Energy", 
            title.size = 1, main.title.position="left")


# Plot 2011 Wind
tm_shape(New_WND_2011)+
  tm_fill("MWh",
          title="Net Generation (in MWh)",
          palette = "YlGn",
          n=4,style="jenks")+ # creates natural breaks
  tm_borders(col="black", lwd=1)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            main.title = "2011 Electricity Generation from Wind Energy", 
            title.size = 1, main.title.position="left")


# Plot 2016 Wind
tm_shape(New_WND_2016)+
  tm_fill("MWh",
          title="Net Generation (in MWh)",
          palette = "YlGn",
          n=4,style="jenks")+ # creates natural breaks
  tm_borders(col="black", lwd=1)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            main.title = "2016 Electricity Generation from Wind Energy", 
            title.size = 1, main.title.position="left")




# Figure #3
# Plot 2021 Wind
# PERCENTILE PLOT
# Creating percentile for breaks in plot
percent <- c(.01,.25,.5,.75,.99,1)
quantile(New_WND_2021$MWh, percent)
break_2021 <- quantile(New_WND_2021$MWh, percent)


tm_shape(New_WND_2021)+
  tm_fill("MWh",
          title="Net Gen Percentile (MWh)",
          palette = "YlGn",
          breaks=break_2021,
          labels=c("1% - %25", "25% - 50%", "50% - 75%","75% - 99%", "> 99%"))+ # creates natural breaks
  tm_borders(col="black", lwd=1)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            main.title = "2021 Electricity Generation from Wind Energy", 
            title.size = 0.5, main.title.position="left")




