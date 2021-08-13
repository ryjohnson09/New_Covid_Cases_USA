# Covid-19 New Cases

library(shiny)
library(lubridate)
library(shinyjs)
library(tidyverse)
library(ggrepel)
library(gt)
library(zoo)

# Get new cases on startup
new_cases_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
    pivot_longer(cols = matches("^\\d"), names_to = "Date", values_to = "Cases") %>% 
    mutate(Date = mdy(Date)) %>% 
    group_by(Province_State, Date) %>% 
    summarise(state_count = sum(Cases)) %>% 
    mutate(new_cases = state_count - lag(state_count)) %>% 
    # Remove negative new_cases (likely reporting correction)
    filter(new_cases >= 0) %>% 
    filter(Date >= today() - months(12)) %>% 
    # Calculate 7 day rolling average
    mutate(roll_avg_7 = rollmeanr(new_cases, 7, fill = NA)) %>% 
    filter(!is.na(roll_avg_7))

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
           plotOutput("new_cases_plot"),
           gt_output("gt_table")
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
        
        new_cases_data %>% 
            filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>% 
            filter(Province_State %in% input$states)
    })
    
    # Plot of new cases
    output$new_cases_plot <- renderPlot({
        
        # Modify for labels
        state_label <- new_cases_filt() %>% 
            group_by(Province_State) %>% 
            filter(roll_avg_7 == max(roll_avg_7)) %>% 
            filter(Date == max(Date))
        
        ggplot(new_cases_filt(), aes(x = Date, y = roll_avg_7)) +
            geom_line(aes(color = Province_State), size = 1.3) +
            theme_minimal() +
            labs(y = "Number of New Cases\n7-day average",
                 title = "New Cases of Covid-19 in the United States") +
            theme(
                legend.position = "none"
            ) +
            geom_label_repel(data = state_label, aes(label = Province_State, color = Province_State))
    })
    
    # Gt table of new cases per week
    output$gt_table <- render_gt({
        # Require input
        req(input$states)
        
        new_cases_data %>% 
            # filter for states/provinces
            filter(Province_State %in% input$states) %>% 
            # Filter for past N weeks
            filter(Date > today() - weeks(5)) %>% 
            group_by(Province_State, week_num = isoweek(Date)) %>% 
            summarise(week_counts = sum(new_cases), .groups = "drop") %>% 
            # Add week start
            mutate(week_start = floor_date(today() - weeks(isoweek(today()) - week_num), 
                                           unit = "week", 
                                           week_start = getOption("lubridate.week.start", 1))) %>% 
            # Change week start format
            mutate(week_start_read = paste0(month(week_start, label = T, abbr = T), " ", day(week_start), ", ", year(week_start))) %>% 
            # gt
            select(Province_State, week_start_read, week_counts) %>% 
            pivot_wider(names_from = week_start_read, values_from = week_counts) %>%
            gt(rowname_col = "Province_State") %>%
            # tab_header(
            #   title = "Number of new Covid Cases by week",
            #   subtitle = "Grouped by State"
            # ) %>%
            tab_footnote(
                footnote = "New cases total by week. Date indicates week start (Monday)",
                locations = cells_column_labels(
                    columns = 2:7
                )
            ) %>%
            cols_align(align = "center") %>%
            tab_style(
                style = list(
                    cell_text(weight = "bold")
                ),
                locations = cells_column_labels(everything())
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
