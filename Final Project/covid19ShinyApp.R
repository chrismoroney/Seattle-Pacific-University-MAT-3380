#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(plotly)
library(shiny)
library(albersusa)

us_COVID19_Data <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv", 
                            col_types = cols(submission_date = col_date(format = "%m/%d/%Y"), 
                                             new_case = col_number(), tot_death = col_number(), 
                                             new_death = col_number())) %>%
    mutate(inc_case_rate = new_case/tot_cases * 100) %>%
    mutate(death_cases = tot_death/tot_cases * 100) %>%
    mutate(inc_case_rate = round(inc_case_rate, digits = 3)) %>%
    mutate(death_cases = round(death_cases, digits = 3)) %>%
    rename(
        "Rate of increased cases (%)" = "inc_case_rate", 
        "Rate of deaths to cases (%)" = "death_cases",
        "Total Cases" = "tot_cases",
        "New Cases" = "new_case",
        "Total Deaths" = "tot_death"
    )

my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}
states <- usa_sf("laea")

covid_by_day <- function(var, date){
    var1 <- enquo(var)
    input_data <- us_COVID19_Data %>% 
        filter(submission_date==as.Date(date)) %>%
        select(state, my_stat =!!var1, submission_date)
    map_data <- left_join(states, input_data, by=c("iso_3166_2"="state")) %>%
        rename(
            "state" = "iso_3166_2"
        ) %>%
        mutate(text = paste(state, ": ", my_stat))
    p <- ggplot(map_data) +
        geom_sf(aes(fill=my_stat, text=text), size=0.25) +
        scale_fill_continuous(low="yellow", high="red") +
        labs(fill=var) +
        my_map_theme()
    ggplotly(p, tooltip="text")
}


# Define UI for generating the map
ui <- fluidPage(

    # Application title
    titlePanel("COVID 19 Map Data"),

    # Sidebar with a slider input for selecting a date for
    sidebarLayout(
        sidebarPanel(
            sliderInput("date",
                        "Select a date to display:",
                        min = as.Date("01/22/2020", "%m/%d/%Y"),
                        max = as.Date("11/20/2020", "%m/%d/%Y"),
                        value = as.Date("06/09/2020", "%m/%d/%Y"), 
                        timeFormat = "%m/%d"), 
            selectInput("var",
                        "Select a variable to plot:",
                        choices = list("Rate of Daily Increased Cases (%)" = "Rate of increased cases (%)",
                                       "Rate of Daily Deaths to Cases (%)" = "Rate of deaths to cases (%)", 
                                       "Total COVID-19 Cases" = "Total Cases",
                                       "New COVID-19 Cases" = "New Cases",
                                       "Total COVID-19-Related Deaths" = "Total Deaths"),
                        selected = "Rate of increased cases (%)")
        ),
        # Show COVID map
        mainPanel(
            h3(textOutput("TitleText")),
            h5(textOutput("SubtitleText")),
            plotlyOutput("map"),
            h5("Data source:", 
               tags$a(href="https://healthdata.gov/dataset/united-states-covid-19-cases-and-deaths-state-over-time", 
                      "United States COVID-19 Cases and Deaths by State over Time")),
            h5("Graph created by Chris Moroney")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$TitleText <- renderText(paste(input$var, " of COVID-19 by State in America"))
    
    output$SubtitleText <- renderText(paste("Graph shows chosen statistics related to COVID-19 in The United 
                                            States Of America"))
    output$map <- renderPlotly({
        covid_by_day(input$var, input$date)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
