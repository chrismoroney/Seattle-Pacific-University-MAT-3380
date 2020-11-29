library(albersusa)
library(tidyverse)

#Recreate rent burden data
library(readxl)
burden <- read_excel("Apartment List Data -- Cost Burden 2019.xlsx", skip = 1)
burden <- select(burden, -12)
namelist <- c("N_Rent_Households", "Overall_Burden_Rate",
              "Moderate_Burden_Rate", "Severe_Burden_Rate",
              "N_Burden_Overall", "N_Burden_Moderate",
              "N_Burden_Severe", "Median_Rent",
              "Median_Renter_Income")
names(burden) <- c("Location", "Type",
                   paste(namelist, "18", sep="_"),
                   paste(namelist, "17", sep="_"),
                   paste(namelist, "08", sep="_"),
                   paste(namelist, "change_17_18", sep="_"),
                   paste(namelist, "change_08_18", sep="_"))

# Save a copy of the dataset for future use
write_csv(burden, "RentCostBurden.csv")

# Create map data 
us_states <- usa_sf()

# Plot map
ggplot(us_states) + 
  geom_sf()

# Recreate map data using Alber's projection
us_states <- usa_sf("laea")
ggplot(us_states) + 
  geom_sf()

# Rent burden data for states only
burden_state <- burden %>%
  filter(Type=="State") %>%
  select(Location, Median_Rent_18)

# Join map data and rent burden data
us_states_burden <- left_join(us_states, burden_state, c("name"="Location"))

# Fill states by rent burden
ggplot(us_states_burden) + 
  geom_sf(aes(fill=Median_Rent_18)) 

# Adjust color of fill; remove background elements
ggplot(us_states_burden) + 
  geom_sf(aes(fill=Median_Rent_18)) +
  scale_fill_continuous(low="yellow", high="red") +
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

# Create theme to remove background elements
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

# Same as previous map, shorter code due to theme
ggplot(us_states_burden) + 
  geom_sf(aes(fill=Median_Rent_18)) +
  scale_fill_continuous(low="yellow", high="red") +
  my_map_theme()

# Final refinements
ggplot(us_states_burden) + 
  geom_sf(aes(fill=Overall_Burden_Rate_18)) +
  scale_fill_continuous(low="yellow", high="red", labels = scales::percent, 
                        breaks=c(0.39,0.56)) +
  guides(fill = guide_colorbar("Rent Cost Burden Rate", title.position="top")) +
  theme(legend.position = c(1, 0), legend.direction = "horizontal") +
  my_map_theme()+
  labs(title="THE UNITED STATES OF HIGH RENT",
       subtitle= "Apartment List looked at the cost of rent relative to income
       and found the percent of households that were \"cost-burdened\"
       with rent higher than 30% of their income.",
       caption = "Source: Apartment List and Yahoo Finance") +
  theme(plot.title = element_text(hjust=0.5, size = 20)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.caption = element_text(hjust=0))

# Final refinements for average rent
ggplot(us_states_burden) + 
  geom_sf(aes(fill=Median_Rent_18)) +
  scale_fill_continuous(low="yellow", high="red", labels = scales::percent, 
                        breaks=c(800, 1600)) +
  guides(fill = guide_colorbar("Average rent", title.position="top")) +
  theme(legend.position = c(1, 0), legend.direction = "horizontal") +
  my_map_theme()+
  labs(title="THE UNITED STATES OF HIGH RENT",
       subtitle= "Apartment List looked at the cost of rent relative to income
       and found the percent of households that were \"cost-burdened\"
       with rent higher than 30% of their income.",
       caption = "Source: Apartment List and Yahoo Finance") +
  theme(plot.title = element_text(hjust=0.5, size = 20)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.caption = element_text(hjust=0))
