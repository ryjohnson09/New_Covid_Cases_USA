library(shiny)
library(lubridate)
library(shinyjs)
library(tidyverse)
library(ggrepel)
library(gt)
library(zoo)
library(pins)
library(httr)

# Read in Data from Pins ------------------------
# Register Board
board_register("rsconnect", server = "https://colorado.rstudio.com/rsc")
# USA totals
usa_cases_deaths <- pin_get("ryan/USA_totals_cases_deaths", board = "rsconnect") 
# State/Province totals
sp_cases_deaths  <- pin_get("ryan/StateProvince_totals_cases_deaths", board = "rsconnect")

pred_time_var <- "2 weeks"

# API call (deaths)
death_pred <-
  httr::GET(
    "https://colorado.rstudio.com/rsc/covid_deaths_pred/pred",
    query = list(pred_time = pred_time_var)
  ) %>%
  httr::content() %>%
  map_df(as_tibble) %>% 
  mutate(.key = factor(.key)) %>% 
  mutate(.index = as.Date(.index))

plot_modeltime_forecast(death_pred,
  .legend_max_width = 25, # For mobile screens
  .interactive      = TRUE, 
  .title = "Forecast: New Covid Deaths",
  .plotly_slider = TRUE,
  .y_lab = "New Deaths: 7-Day Average"
)


# API call (cases)
cases_pred <-
  httr::GET(
    "https://colorado.rstudio.com/rsc/covid_cases_pred/pred",
    query = list(pred_time = pred_time_var)
  ) %>%
  httr::content() %>%
  map_df(as_tibble) %>% 
  mutate(.key = factor(.key)) %>% 
  mutate(.index = as.Date(.index))

plot_modeltime_forecast(cases_pred,
                        .legend_max_width = 25, # For mobile screens
                        .interactive      = TRUE, 
                        .title = "Forecast: New Covid Cases",
                        .plotly_slider = TRUE,
                        .y_lab = "New Cases: 7-Day Average"
)

