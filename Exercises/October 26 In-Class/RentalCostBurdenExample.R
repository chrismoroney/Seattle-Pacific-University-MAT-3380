# Website for data: https://www.apartmentlist.com/rentonomics/cost-burden-2019/
# Download the data as an Excel file
# Set your working directory to the folder containing the file

# Import data:

library(readxl)
library(tidyverse)
library(plotly)

burden <- read_excel("Apartment List Data -- Cost Burden 2019.xlsx", skip = 1)
burden <- select(burden, -12)
View(burden)

# Rename variables:
namelist <- c("N_Rent_Households",
              "Overall_Burden_Rate",
              "Moderate_Burden_Rate",
              "Severe_Burden_Rate",
              "N_Burden_Overall",
              "N_Burden_Moderate",
              "N_Burden_Severe",
              "Median_Rent",
              "Median_Renter_Income")
paste(namelist, "18", sep="_")
names(burden)
names(burden) <- c("Location",
                   "Type",
                   paste(namelist, "18", sep="_"),
                   paste(namelist, "17", sep="_"),
                   paste(namelist, "08", sep="_"),
                   paste(namelist, "change_17_18", sep="_"),
                   paste(namelist, "change_08_18", sep="_"))
names(burden)

# First version of plot:
myplot <- burden %>%
  filter(Type == "Metro") %>%
  ggplot(aes(x=Overall_Burden_Rate_18, y=N_Burden_Overall_18, ids=Location)) +
  geom_point(aes(color=Overall_Burden_Rate_18)) +
  scale_y_log10() +
  labs(title="THE U.S. CITIES WITH\nTHE BIGGEST COST BURDENS",
       subtitle=
         "Apartment List analyzed which cities have the worst income-to-rent ratios.",
       caption = "Source: Apartment List and Yahoo Finance") +
  xlab("OVERALL COST BURDEN RATE") +
  ylab("# OF COST BURDENED HOUSEHOLDS")

myplot

# Identify outlier and source of warning?
View(filter(burden, Overall_Burden_Rate_18 == 0 & Type=="Metro"))

## Edit ggplot command above to remove Twin Falls
myplot <- burden %>%
  filter(Type == "Metro" & Overall_Burden_Rate_18 > 0) %>%
  ggplot(aes(x=Overall_Burden_Rate_18, y=N_Burden_Overall_18)) +
  geom_point(aes(color=Overall_Burden_Rate_18)) +
  scale_y_log10() +
  labs(title="THE U.S. CITIES WITH\nTHE BIGGEST COST BURDENS",
       subtitle=
         "Apartment List analyzed which cities have the worst income-to-rent ratios.",
       caption = "Source: Apartment List and Yahoo Finance") +
  xlab("OVERALL COST BURDEN RATE") +
  ylab("# OF COST BURDENED HOUSEHOLDS")
myplot
######

# Match point colors from original plot and remove legend
myplot <- myplot +
  scale_color_gradient(low="yellow", high="red") +
  guides(color=FALSE) 
myplot

# Change background colors
myplot <- myplot +
  theme(plot.background = element_rect(fill = "#204c66")) +
  theme(panel.background = element_rect(fill = "#204c66")) 
myplot

# Adjust color of labels and center titles
myplot <- myplot +
  theme(title = element_text(color = "white")) +
  theme(axis.text = element_text(color = "white")) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(plot.subtitle = element_text(hjust=0.5))
myplot

# Label select cities
named_cities <- burden %>%
  filter(Location %in% c("New York, NY",
                         "Los Angeles, CA",
                         "San Francisco, CA",
                         "San Diego, CA",
                         "Miami, FL",
                         "Riverside, CA",
                         "New Orleans, LA",
                         "Vineland, NJ",
                         "Ocean City, NJ")) 

named_cities <- filter(named_cities, Type == "Metro")

myplot +
  geom_text(data = named_cities, 
            aes(x = Overall_Burden_Rate_18, y = N_Burden_Overall_18, 
                label=Location), 
            color="white")

## Make modifications to the command above to control position of labels
# First try:
myplot +
  geom_text(data = named_cities, 
            aes(x = Overall_Burden_Rate_18, y = N_Burden_Overall_18, 
                label=Location), 
            color="white", hjust="outward")

# Second try:
myplot +
  geom_text(data = named_cities, 
            aes(x = Overall_Burden_Rate_18, y = N_Burden_Overall_18, 
                label=paste(" ", Location, " ")), 
            color="white", hjust="outward")

# Third try:
myplot +
  geom_text(data=named_cities, 
            aes(x=Overall_Burden_Rate_18, y=N_Burden_Overall_18, 
                label=paste(" ", Location, " ")), size = 3,
            color="white", hjust="outward", 
            vjust=c(.5, .5, .5, .5, .5, 1, 0, .5, .5))

# Final version with adjusted x-axis scale:
myplot <- myplot +
  geom_text(data=named_cities, 
            aes(x=Overall_Burden_Rate_18, y=N_Burden_Overall_18, 
                label=paste(" ", Location, " ")), size = 3,
            color="white", hjust="outward", 
            vjust=c(.5, .5, .5, .5, .5, 1, 0, .5, .5)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.2, 0.8))
myplot

# y-axis labels:
myplot +
  scale_y_log10(breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 
                           100000, 200000, 500000, 1000000),
                labels = c("1000", "2K", "5K", "10K", "20K", "50K", 
                           "100K", "200K", "500K", "1M"))

# Remove extra gridline:
myplot +
  scale_y_log10(breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 
                           100000, 200000, 500000, 1000000),
                labels = c("1000", "2K", "5K", "10K", "20K", "50K", 
                           "100K", "200K", "500K", "1M"),
                minor_breaks=NULL)

## Put it all together:
myplot <- burden %>%
  filter(Type == "Metro" & Overall_Burden_Rate_18 > 0) %>%
  ggplot(aes(x=Overall_Burden_Rate_18, y=N_Burden_Overall_18, ids=Location,
             text=paste("Burden Rate: ",Overall_Burden_Rate_18 * 100,"%"))) +
  geom_point(aes(color=Median_Renter_Income_18)) +
  scale_y_log10(breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 
                           100000, 200000, 500000, 1000000),
                labels = c("1000", "2K", "5K", "10K", "20K", "50K", 
                           "100K", "200K", "500K", "1M"),
                minor_breaks=NULL) +
  labs(title="The U.S. Cities with\nThe Biggest Cost Burdens",
       subtitle=
         "Apartment List analyzed which cities have the worst income-to-rent ratios.",
       caption = "Source: Apartment List and Yahoo Finance") +
  xlab("Overall Cost Burden Rate") +
  ylab("# of Cost Burdened Households") +
  labs(color = "Median Renter Income 2018") +
  scale_color_gradient(low="red", high="green") +
  theme(plot.background = element_rect(fill = "#1c3078")) +
  theme(panel.background = element_rect(fill = "#1c3078")) +
  theme(legend.background = element_rect(fill = "#1c3078")) +
  theme(legend.text = element_text(color = "white")) +
  theme(title = element_text(color = "white")) +
  theme(axis.text = element_text(color = "white")) +
  theme(plot.title = element_text(hjust=0.5, size = 25)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.2, 0.8)) +
  theme(panel.grid = element_line(color = "white", size = 0.1))
myplot

ggplotly(myplot, tooltip = c("ids", "text"))

# Save the graph
png("myplot.png", width=6, height=6, res=300, units="in")
myplot
dev.off()




