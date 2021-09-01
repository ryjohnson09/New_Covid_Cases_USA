library(plumber)
library(pins)
library(modeltime)
library(tidyverse)

# Red in data ------------------------------------
usa_total_cases <- pin_get("ryan/USA_totals_cases_deaths", board = "rsconnect") %>% 
  select(Date, total_cases_avg_7)

# Get model -------------------------------------
cases_model <- pin_get("ryan/Covid_model_cases", board = "rsconnect")
# coerce to mdl_time_tbl
cases_model_1 <- structure(cases_model, class = c("mdl_time_tbl", "tbl_df", "tbl", "data.frame"))

# API -------------------------------------------
#* @apiTitle Predict 7-day average for new cases of Covid-19

#* Predict new 7-day average
#* @param pred_time Length of time to make predictions
#* @get /pred
function(pred_time) {
  modeltime_forecast(cases_model_1, h = pred_time,  actual_data = usa_total_cases)
}