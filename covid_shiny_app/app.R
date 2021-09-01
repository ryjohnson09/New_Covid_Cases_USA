# Covid-19 New Cases

library(shiny)
library(lubridate)
library(shinyjs)
library(tidyverse)
library(ggrepel)
library(gt)
library(zoo)
library(pins)
library(plotly)
library(modeltime)

# Read in Data from Pins ------------------------
# Register Board
board_register("rsconnect", server = "https://colorado.rstudio.com/rsc")
# USA totals
usa_cases_deaths <- pin_get("ryan/USA_totals_cases_deaths", board = "rsconnect") 
# State/Province totals
sp_cases_deaths  <- pin_get("ryan/StateProvince_totals_cases_deaths", board = "rsconnect")


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
                        start = min(sp_cases_deaths$Date),
                        end = max(sp_cases_deaths$Date)
                        ),
            selectizeInput("states",
                           "State/Province",
                           choices = unique(sp_cases_deaths$Province_State),
                           multiple = TRUE,
                           options = list(placeholder = 'Select State/Province'), 
                           selected = "Maryland"
            ),
            actionButton("resetStates", "Reset")
        ),

        # Show Plot
        mainPanel(
           plotlyOutput("sp_cases_deaths_plot"),
           plotlyOutput("USA_cases_deaths_plot")
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
        
        # Require input
        req(input$states)
        
        sp_cases_deaths %>% 
            # filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>% 
            filter(Province_State %in% input$states)
    })
    
    # Plot of new cases/deaths by state/province
    output$sp_cases_deaths_plot <- renderPlotly({
        
        # Modify for labels
        state_label <- new_cases_filt() %>% 
            group_by(Province_State) %>% 
            filter(cases_avg_7 == max(cases_avg_7, na.rm = TRUE)) %>% 
            filter(Date == max(Date))
        
        ggplotly(ggplot(new_cases_filt(), aes(x = Date, y = cases_avg_7)) +
            geom_blank() +
            geom_line(aes(color = Province_State), size = 1) +
            theme_minimal() +
            labs(y = "Number of New Cases\n7-day average",
                 title = "New Cases of Covid-19 by State/Province") +
            theme(
                legend.position = "none"
            ))
            #geom_label_repel(data = state_label, aes(label = Province_State, color = Province_State))
        
    })
    
    # USA New Cases and Deaths Plot
    output$USA_cases_deaths_plot <- renderPlotly({
        
        # API call (deaths)
        death_pred <-
            httr::GET(
                "https://colorado.rstudio.com/rsc/covid_deaths_pred/pred",
                query = list(pred_time = "3 weeks")
            ) %>%
            httr::content() %>%
            map_df(as_tibble) %>% 
            mutate(.key = factor(.key)) %>% 
            mutate(.index = as.Date(.index))
        
        # Plot using modeltime package
        plot_modeltime_forecast(death_pred,
                                .legend_max_width = 25, # For mobile screens
                                .interactive      = TRUE, 
                                .title = "Forecast: New Covid Deaths",
                                .plotly_slider = TRUE,
                                .y_lab = "New Deaths: 7-Day Average"
        )
    })
}
    

# Run the application 
shinyApp(ui = ui, server = server)
