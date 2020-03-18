library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

covid_raw <- read.csv("https://covid.ourworldindata.org/data/full_data.csv", stringsAsFactors = FALSE)
covid_data <- covid_raw %>%
    rename(Country.Region = location) %>%
    mutate(date = ymd(date))
    
country <- unique(covid_data$Country.Region)
earliest_date <- min(covid_data$date)
latest_date <- max(covid_data$date)
data_source <- "Data from https://ourworldindata.org/coronavirus-source-data"

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            p(paste0("Data last updated ", latest_date, ".")),
            selectizeInput("country_select", "Select countries to plot (you may select multiple)", 
                           choices = list("Type in a country" = "", "Countries" = country), 
                           selected = "United Kingdom", 
                           multiple = TRUE,
                           options = NULL),
            dateRangeInput("date_select", "Select date range", 
                           start = earliest_date, end = latest_date, 
                           min = earliest_date, max = latest_date),
            p("NB: This site previously used the data maintained by the Johns Hopkins Coronavirus Resource Center, but has recently been changed to a different source."),
            a(href = "https://ourworldindata.org/coronavirus-source-data", "Data is maintained by Our World in Data (as of 2020-03-18)."),
            p(""),
            a(href = "https://github.com/dannyjnwong/COVID", "Code for the app is available here: https://github.com/dannyjnwong/COVID")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Cumulative Plots", 
                                 plotOutput("Cases"),
                                 plotOutput("Deaths")),
                        tabPanel("Summary", 
                                 tableOutput("Summary")),
                        tabPanel("Offset Dates (Cases)", 
                                 sliderInput("offset_value", "Select minimum case number corresponding to Day 1", 
                                             min = 10, max = 500,
                                             value = 50,
                                             round = TRUE, 
                                             width = "100%"),
                                 plotOutput("OffsetCases"), 
                                 plotOutput("LogOffsetCases")),
                        tabPanel("Offset Dates (Deaths)", 
                                 sliderInput("offset_deaths_value", "Select minimum cumulative deaths corresponding to Day 1", 
                                             min = 00, max = 500,
                                             value = 10,
                                             round = TRUE, 
                                             width = "100%"),
                                 plotOutput("OffsetDeaths"), 
                                 plotOutput("LogOffsetDeaths"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Cases <- renderPlot({
        covid_data %>% 
            filter(Country.Region %in% input$country_select) %>%
            filter(date >= input$date_select[1], date <= input$date_select[2]) %>%
            ggplot(aes(x = date, y = total_cases, col = Country.Region)) + 
            geom_line() +
            labs(x = "Date", 
                 y = "Cases",
                 title = "Cumulative frequency of confirmed cases",
                 subtitle = data_source,
                 col = "Country") +
            scale_x_date(labels = scales::date_format("%d-%m-%Y")) +
            theme_bw() +
            theme(legend.position="bottom")
        
    })
    
    output$Deaths <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(date >= input$date_select[1], date <= input$date_select[2]) %>%
            ggplot(aes(x = date, y = total_deaths, col = Country.Region)) + 
            geom_line() +
            labs(x = "Date", 
                 y = "Cases",
                 title = "Cumulative deaths",
                 subtitle = data_source,
                 col = "Country") +
            scale_x_date(labels = scales::date_format("%d-%m-%Y")) +
            theme_bw() +
            theme(legend.position="bottom")
        
    })
    
    output$Summary <- renderTable({
        covid_data %>% filter(Country.Region %in% input$country_select) %>%
            rename(Country = Country.Region) %>%
            group_by(Country) %>%
            summarise(`Current confirmed cases` = max(total_cases, na.rm = TRUE),
                      `Highest single day rise` = max(new_cases, na.rm = TRUE)) -> confirmed_cases
        
        covid_data %>% filter(Country.Region %in% input$country_select) %>%
            rename(Country = Country.Region) %>%
            group_by(Country) %>%
            summarise(`Highest single day deaths` = max(new_deaths, na.rm = TRUE),
                      `Current deaths` = max(total_deaths, na.rm = TRUE)) -> confirmed_deaths
        tbl <- left_join(confirmed_cases, confirmed_deaths, by = "Country") %>%
            arrange(desc(`Current confirmed cases`))
        tbl
    })
    
    output$OffsetCases <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(total_cases >= input$offset_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = total_cases, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Cases",
                 title = "Cumulative frequency of confirmed cases with date offset",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$LogOffsetCases <- renderPlot({
        covid_data %>% 
            filter(Country.Region %in% input$country_select) %>%
            filter(total_cases >= input$offset_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = total_cases, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Cases",
                 title = "Cumulative frequency of confirmed cases with date offset (log scale)",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$OffsetDeaths <- renderPlot({
        covid_data %>% 
            filter(Country.Region %in% input$country_select) %>%
            filter(total_deaths >= input$offset_deaths_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = total_deaths, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative deaths"), 
                 y = "Deaths",
                 title = "Cumulative frequency of deaths with date offset",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$LogOffsetDeaths <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(total_deaths >= input$offset_deaths_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = total_deaths, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative deaths"), 
                 y = "Deaths",
                 title = "Cumulative frequency of deaths with date offset (log scale)",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10") +
            theme_bw() +
            theme(legend.position="bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
