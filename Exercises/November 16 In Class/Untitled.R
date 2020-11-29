# https://covidtracking.com/data/download/all-states-history.csv
library(shiny)
library(tidyverse)
library(albersusa)
library(plotly)
uscovid <- read_csv("https://covidtracking.com/data/download/all-states-history.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))


alldata <- usa_sf("laea") %>%
  select(iso_3166_2) %>%
  left_join(uscovid, by = c("iso_3166_2" = "state"))

covidmap <- function(mydate){
  map <- alldata %>% 
    filter(date == as.Date(mydate)) %>%
    ggplot() + geom_sf(aes(fill=death))
  ggplotly(map)
}
covidmap("2020-11-15")
covidmap("2020-10-31")
covidmap("2020-03-10")