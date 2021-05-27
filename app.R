# Covid-19 New Cases

library(shiny)
library(lubridate)
library(shinyjs)
library(tidyverse)
library(ggrepel)

# Get new cases on startup
new_cases_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
    pivot_longer(cols = matches("^\\d"), names_to = "Date", values_to = "Cases") %>% 
    mutate(Date = mdy(Date)) %>% 
    group_by(Province_State, Date) %>% 
    summarise(state_count = sum(Cases)) %>% 
    mutate(new_cases = state_count - lag(state_count)) %>% 
    # Remove negative new_cases (likely reporting correction)
    filter(new_cases >= 0)

# Define UI -------------------------------------
ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("New Covid-19 Cases in USA"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("date_range",
                        "Date Range:",
                        start = "2020-01-01",
                        end = today()
                        ),
            selectizeInput("states",
                           "State/Province",
                           choices = unique(new_cases_data$Province_State),
                           multiple = TRUE,
                           options = list(placeholder = 'Select State/Province')
            ),
            actionButton("resetStates", "Reset")
        ),

        # Show Plot
        mainPanel(
           plotOutput("new_cases_plot")
        )
    )
)

# Define server -------------------------------------
server <- function(input, output) {
    
    observeEvent(input$resetStates, {
        reset("states")
    })
    
    # Filter new cases based on inputs
    new_cases_filt <- reactive({
        new_cases_data %>% 
            filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>% 
            filter(Province_State %in% input$states)
    })

    output$new_cases_plot <- renderPlot({
        
        # Modify for labels
        state_label <- new_cases_filt() %>% 
            group_by(Province_State) %>% 
            filter(new_cases == max(new_cases)) %>% 
            filter(Date == max(Date))
        
        ggplot(new_cases_filt(), aes(x = Date, y = new_cases)) +
            geom_line(aes(color = Province_State)) +
            theme_minimal() +
            labs(y = "Number of New Cases",
                 title = "New Cases of Covid-19 in the United States") +
            theme(
                legend.position = "none"
            ) +
            geom_label_repel(data = state_label, aes(label = Province_State, color = Province_State))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
