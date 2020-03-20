library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

population <- read.csv("data/population.csv", skip = 4, stringsAsFactors = FALSE) %>%
    rename(Country.Region = Country.Name) %>%
    mutate(Country.Region = recode(Country.Region, `Bahamas, The` = "Bahamas",
                                   `Brunei Darussalam` = "Brunei",
                                   `Congo, Rep.` = "Congo",
                                   `Congo, Dem. Rep.` = "Democratic Republic of Congo",
                                   `Egypt, Arab Rep.` = "Egypt",
                                   `Eswatini` = "Swaziland",
                                   `Faroe Islands` = "Faeroe Islands",
                                   `Iran, Islamic Rep.` = "Iran",
                                   `North Macedonia` = "Macedonia",
                                   `Korea, Rep.` = "South Korea",
                                   `Russian Federation` = "Russia",
                                   `St. Lucia` = "Saint Lucia",
                                   `St. Martin (French part)` = "Saint Martin (French part)",
                                   `St. Vincent and the Grenadines` = "Saint Vincent and the Grenadines",
                                   `Slovak Republic` = "Slovakia",
                                   `Virgin Islands (U.S.)` = "United States Virgin Islands",
                                   `Venezuela, RB` = "Venezuela")) %>%
    select(Country.Region, population = X2018)

covid_raw <- read.csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv", stringsAsFactors = FALSE)
covid_data <- covid_raw %>%
    rename(Country.Region = location) %>%
    mutate(date = ymd(date)) %>%
    left_join(population, by = "Country.Region") %>%
    mutate(cases_per_capita = total_cases/population * 100000) %>%
    mutate(deaths_per_capita = total_deaths/population * 100000)

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
                                 h2("Unadjusted for population"),
                                 plotOutput("Cases"),
                                 plotOutput("Deaths"),
                                 hr(),
                                 h2("Adjusted for population"),
                                 p("The following plots have been adjusted for population size in each country."),
                                 a(href = "https://data.worldbank.org/", "Population data from the World Bank."),
                                 plotOutput("CasesPerCapita"),
                                 plotOutput("DeathsPerCapita")),
                        tabPanel("Summary", 
                                 tableOutput("Summary")),
                        tabPanel("Offset Dates (Cases)", 
                                 sliderInput("offset_value", "Select minimum case number corresponding to Day 0", 
                                             min = 10, max = 500,
                                             value = 50,
                                             round = TRUE, 
                                             width = "100%"),
                                 h2("Unadjusted for population"),
                                 plotOutput("OffsetCases"), 
                                 plotOutput("LogOffsetCases"),
                                 hr(),
                                 h2("Adjusted for population"),
                                 p("The following plots have been adjusted for population size in each country."),
                                 a(href = "https://data.worldbank.org/", "Population data from the World Bank."),
                                 plotOutput("OffsetCasesPerCapita"), 
                                 plotOutput("LogOffsetCasesPerCapita")),
                        tabPanel("Offset Dates (Deaths)", 
                                 sliderInput("offset_deaths_value", "Select minimum cumulative deaths corresponding to Day 0", 
                                             min = 00, max = 500,
                                             value = 10,
                                             round = TRUE, 
                                             width = "100%"),
                                 h2("Unadjusted for population"),
                                 plotOutput("OffsetDeaths"), 
                                 plotOutput("LogOffsetDeaths"),
                                 hr(),
                                 h2("Adjusted for population"),
                                 p("The following plots have been adjusted for population size in each country."),
                                 a(href = "https://data.worldbank.org/", "Population data from the World Bank."),
                                 plotOutput("OffsetDeathsPerCapita"), 
                                 plotOutput("LogOffsetDeathsPerCapita")),
                        tabPanel("Forecasts", 
                                 h1("WARNING: The forecasts in this tab are highly experimental! Interpret with care."),
                                 sliderInput("offset_forecast_deaths_value", "Select minimum cumulative deaths corresponding to Day 0", 
                                             min = 00, max = 500,
                                             value = 10,
                                             round = TRUE, 
                                             width = "100%"),
                                 h2("Unadjusted for population"),
                                 #plotOutput("ForecastCases"),
                                 plotOutput("ForecastDeaths"),
                                 hr(),
                                 h2("Adjusted for population"),
                                 p("The following plots have been adjusted for population size in each country."),
                                 a(href = "https://data.worldbank.org/", "Population data from the World Bank."),
                                 #plotOutput("ForecastCasesPerCapita"),
                                 plotOutput("ForecastDeathsPerCapita"))
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
                 y = "Deaths",
                 title = "Cumulative deaths",
                 subtitle = data_source,
                 col = "Country") +
            scale_x_date(labels = scales::date_format("%d-%m-%Y")) +
            theme_bw() +
            theme(legend.position="bottom")
        
    })
    
    output$CasesPerCapita <- renderPlot({
        covid_data %>% 
            filter(Country.Region %in% input$country_select) %>%
            filter(date >= input$date_select[1], date <= input$date_select[2]) %>%
            ggplot(aes(x = date, y = cases_per_capita, col = Country.Region)) + 
            geom_line() +
            labs(x = "Date", 
                 y = "Cases per 100,000 population",
                 title = "Cumulative frequency of confirmed cases per capita",
                 subtitle = data_source,
                 col = "Country") +
            scale_x_date(labels = scales::date_format("%d-%m-%Y")) +
            theme_bw() +
            theme(legend.position="bottom")
        
    })
    
    output$DeathsPerCapita <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(date >= input$date_select[1], date <= input$date_select[2]) %>%
            ggplot(aes(x = date, y = deaths_per_capita, col = Country.Region)) + 
            geom_line() +
            labs(x = "Date", 
                 y = "Deaths per 100,000 population",
                 title = "Cumulative deaths per capita",
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
    
    output$OffsetCasesPerCapita <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(total_cases >= input$offset_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = cases_per_capita, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Cases per 100,000 population",
                 title = "Cumulative frequency of confirmed cases per capita with date offset",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$LogOffsetCasesPerCapita <- renderPlot({
        covid_data %>% 
            filter(Country.Region %in% input$country_select) %>%
            filter(total_cases >= input$offset_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = cases_per_capita, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_value, "cumulative cases"), 
                 y = "Cases per 100,000 population",
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
            labs(x = paste("Day number since", input$offset_deaths_value, "cumulative deaths"), 
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
            labs(x = paste("Day number since", input$offset_deaths_value, "cumulative deaths"), 
                 y = "Deaths",
                 title = "Cumulative frequency of deaths with date offset (log scale)",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$OffsetDeathsPerCapita <- renderPlot({
        covid_data %>% 
            filter(Country.Region %in% input$country_select) %>%
            filter(total_deaths >= input$offset_deaths_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = deaths_per_capita, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_deaths_value, "cumulative deaths"), 
                 y = "Deaths per 100,000 population",
                 title = "Cumulative frequency of deaths with date offset",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            theme_bw() +
            theme(legend.position="bottom")
    })
    
    output$LogOffsetDeathsPerCapita <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(total_deaths >= input$offset_deaths_value) %>% 
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = deaths_per_capita, group = Country.Region)) + 
            geom_line(aes(linetype = Country.Region, col = Country.Region)) +
            labs(x = paste("Day number since", input$offset_deaths_value, "cumulative deaths"), 
                 y = "Deaths per 100,000 population",
                 title = "Cumulative frequency of deaths with date offset (log scale)",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10") +
            theme_bw() +
            theme(legend.position="bottom")
    })

    
    output$ForecastDeaths <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(total_deaths >= input$offset_forecast_deaths_value) %>%
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = total_deaths, col = Country.Region)) + 
            geom_point() +
            geom_smooth(method = "lm", fullrange = TRUE) +
            labs(x = paste("Day number since", input$offset_forecast_deaths_value, "cumulative deaths"), 
                 y = "Deaths",
                 title = "Forecasted deaths with date offset",
                 subtitle = data_source,
                 col = "Country") +
            scale_y_continuous(trans = "log10", labels = scales::comma_format()) +
            scale_colour_brewer(palette = "Set1") +
            theme_bw() +
            theme(legend.position="bottom")
    })
        
    output$ForecastDeathsPerCapita <- renderPlot({
        covid_data %>%
            filter(Country.Region %in% input$country_select) %>%
            filter(total_deaths >= input$offset_forecast_deaths_value) %>%
            group_by(Country.Region) %>% 
            mutate(Days = seq(from = 0, to = n()-1)) %>%
            ggplot(aes(x = Days, y = deaths_per_capita, col = Country.Region)) + 
            geom_point() +
            geom_smooth(method = "lm", fullrange = TRUE) +
            labs(x = paste("Day number since", input$offset_forecast_deaths_value, "cumulative deaths"), 
                 y = "Deaths per 100,000 population",
                 title = "Forecasted deaths with date offset (log scale)",
                 subtitle = data_source,
                 col = "Country",
                 linetype = "Country") +
            scale_y_continuous(trans = "log10", labels = scales::comma_format(accuracy = 0.01)) +
            scale_colour_brewer(palette = "Set1") +
            theme_bw() +
            theme(legend.position="bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
