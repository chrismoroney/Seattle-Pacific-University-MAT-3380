# Reload things from previous script (can probably skip these commands)
library(tidyverse)
library(albersusa)
burden <- read_csv("RentCostBurden.csv")
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}
#####

# Map of county boundaries in US
us_county <- counties_sf("laea")
ggplot(us_county) + 
  geom_sf(size=0.25) +
  my_map_theme()

# Prepare county-level rent burden data and merge with map data
burden_county <- burden %>%
  filter(Type=="County") %>%
  select(Location, Overall_Burden_Rate_18) %>%
  separate(Location, c("county", "state"), sep=", ") 

us_county_burden <- left_join(us_county, burden_county,
                              by=c("name"="county","iso_3166_2"="state")) 

# Create choropleth
ggplot(us_county_burden) + 
  geom_sf(aes(fill=Overall_Burden_Rate_18), size=0.25) +
  scale_fill_continuous(low="yellow", high="red") +
  my_map_theme()