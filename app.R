library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

covid_raw <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv", stringsAsFactors = FALSE)
covid_data <- covid_raw %>%
    group_by(Country.Region, type, date) %>%
    summarise(daily = sum(cases)) %>%
    mutate(cumul_freq = cumsum(daily)) %>%
    mutate(date = ymd(date))
    
country <- unique(covid_data$Country.Region)
earliest_date <- min(covid_data$date)
latest_date <- max(covid_data$date)

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
            a(href = "https://github.com/CSSEGISandData/COVID-19", "Data is maintained by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University."),
            p(""),
            a(href = "https://ramikrispin.github.io/coronavirus/", "Thanks to Rami Krispin for parsing the data into a tidy format.")
            
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
        covid_data %>% filter(type == "confirmed") %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(date >= input$date_select[1], date <= input$date_select[2]) %>%
            ggplot(aes(x = date, y = cumul_freq, col = Country.Region)) + 
            geom_line() +
            labs(x = "Date", 
                 y = "Cases",
                 title = "Cumulative frequency of confirmed cases",
                 subtitle = "Data from the 2019 Novel Coronavirus Visual Dashboard\nJohns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 col = "Country") +
            scale_x_date(labels = scales::date_format("%d-%m-%Y")) +
            theme_bw() +
            theme(legend.position="bottom")
        
    })
    
    output$Deaths <- renderPlot({
        covid_data %>% filter(type == "death") %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(date >= input$date_select[1], date <= input$date_select[2]) %>%
            ggplot(aes(x = date, y = cumul_freq, col = Country.Region)) + 
            geom_line() +
            labs(x = "Date", 
                 y = "Cases",
                 title = "Cumulative deaths",
                 subtitle = "Data from the 2019 Novel Coronavirus Visual Dashboard\nJohns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 col = "Country") +
            scale_x_date(labels = scales::date_format("%d-%m-%Y")) +
            theme_bw() +
            theme(legend.position="bottom")
        
    })
    
    output$Summary <- renderTable({
        covid_data %>% filter(Country.Region %in% input$country_select) %>%
            rename(Country = Country.Region) %>%
            filter(type == "confirmed") %>%
            group_by(Country) %>%
            summarise(`Current confirmed cases` = max(cumul_freq),
                      `Highest single day rise` = max(daily)) -> confirmed_cases
        
        covid_data %>% filter(Country.Region %in% input$country_select) %>%
            rename(Country = Country.Region) %>%
            filter(type == "death") %>%
            group_by(Country) %>%
            summarise(`Current deaths` = max(cumul_freq)) -> confirmed_deaths
        tbl <- left_join(confirmed_cases, confirmed_deaths, by = "Country") %>%
            arrange(desc(`Current confirmed cases`))
        tbl
    })
    
    output$OffsetCases <- renderPlot({
        covid_data %>% filter(type == "confirmed") %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(cumul_freq >= input$offset_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 1, to = n())) %>%
            ggplot(aes(x = Days, y = cumul_freq, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Cases",
                 title = "Cumulative frequency of confirmed cases with date offset",
                 subtitle = "Data from the 2019 Novel Coronavirus Visual Dashboard\nJohns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 col = "Country",
                 linetype = "Country") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$LogOffsetCases <- renderPlot({
        covid_data %>% filter(type == "confirmed") %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(cumul_freq >= input$offset_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 1, to = n())) %>%
            ggplot(aes(x = Days, y = cumul_freq, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Cases",
                 title = "Cumulative frequency of confirmed cases with date offset (log scale)",
                 subtitle = "Data from the 2019 Novel Coronavirus Visual Dashboard\nJohns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$OffsetDeaths <- renderPlot({
        covid_data %>% filter(type == "confirmed") %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(cumul_freq >= input$offset_deaths_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 1, to = n())) %>%
            ggplot(aes(x = Days, y = cumul_freq, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Deaths",
                 title = "Cumulative frequency of confirmed deaths with date offset",
                 subtitle = "Data from the 2019 Novel Coronavirus Visual Dashboard\nJohns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 col = "Country",
                 linetype = "Country") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$LogOffsetDeaths <- renderPlot({
        covid_data %>% filter(type == "confirmed") %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(cumul_freq >= input$offset_deaths_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 1, to = n())) %>%
            ggplot(aes(x = Days, y = cumul_freq, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Deaths",
                 title = "Cumulative frequency of confirmed cases with date offset (log scale)",
                 subtitle = "Data from the 2019 Novel Coronavirus Visual Dashboard\nJohns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10") +
            theme_bw() +
            theme(legend.position="bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
