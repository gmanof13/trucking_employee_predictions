library(tidyverse)
library(fredr)
library(ggthemes)
library(timetk)
library(zoo)
library(janitor)
library(Hmisc)
library(corrplot)
library(modeltime)
library(rsample)
library(generics)
library(parsnip)
library(recipes)
library(tidymodels)
library(modeltime.ensemble)
library(gghighlight)
library(keras)

# getting the data ready  

fredr_set_key("3f279f02c757808ef45fa93d58384232")

popular_funds_series <- 
  fredr_series_search_text(
  search_text = "transportation",
  order_by = "popularity",
  sort_order = "desc"
)

truck_employee_fred_id <- 
  popular_funds_series %>% 
  filter(title == "All Employees, Truck Transportation") %>% 
  filter(id == 'CEU4348400001') %>% 
  pull(id)


truck_employee_df <- 
  truck_employee_fred_id %>% 
  fredr(
    observation_start = as.Date("2010-01-01"),
    observation_end = as.Date("2022-12-31")
  ) %>% 
  select(date,value) %>% 
  mutate(value = value * 1000)

total_manuf_id <- 'UMTMNO'

total_manuf_df <- 
  total_manuf_id %>% 
  fredr(
    observation_start = as.Date("2010-01-01"),
    observation_end = as.Date("2022-12-31")
  ) %>% 
  select(date,value)

employee_manuf_combined <-
  truck_employee_df %>% 
  left_join(total_manuf_df,by="date") %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  rename(new_orders = value_y,trucking_employees=value_x)

# visualizing trucking employees 

employee_manuf_combined %>% timetk::plot_stl_diagnostics(.date_var = date,.value=trucking_employees)

# cor_results %>% corrplot(method='color')


# modeltime 

interactive <- FALSE

model_df <- 
  employee_manuf_combined %>% 
  mutate(value = trucking_employees) %>% 
  select(date,value) %>% 
  drop_na()


splits <- initial_time_split(model_df)


# Model 1: arima_boost 

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))


# Model 2: ets 

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))


# Model 3: prophet

model_fit_prophet <- prophet_reg(seasonality_yearly= TRUE,
                                 seasonality_weekly=TRUE,
                                 seasonality_daily= FALSE,
                                 season = "multiplicative",
                                 changepoint_range = .3) %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))



model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))



# all models 

models_tbl <- 
  modeltime_table(
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm
)


calibration_tbl <- models_tbl %>% 
  modeltime_calibrate(new_data = testing(splits))

# model accuracy table

model_accuracy <- 
calibration_tbl %>%
  modeltime_accuracy() 


model_accuracy %>% 
  arrange(mae)

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = model_df)

model_predictions <- 
refit_tbl %>%
  modeltime_forecast(h = "1 years", actual_data = model_df)


# new data 

truck_employee_df_new <- 
  truck_employee_fred_id %>% 
  fredr(
    observation_start = as.Date("2023-01-01"),
    observation_end = as.Date("2023-12-31")
  ) %>% 
  select(date,value) %>% 
  mutate(value = value * 1000) %>% 
  mutate(.model_desc = "ACTUAL") %>% 
  rename(.index = date,
         .value = value)

# comparing model to new data 

model_predictions %>% 
  filter(year(.index) >= 2023) %>% 
  select(.index,.value,.model_desc) %>% 
  rbind(truck_employee_df_new) %>% 
  ggplot(aes(
    x=.index,
    y=.value,
    color=.model_desc
  ))+
  geom_line()+
  gghighlight()+
  ggthemes::theme_clean()+
  scale_x_date(date_labels="%b-%y",date_breaks  ="1 month")+
  labs(
    title = "Trucking Employee Predictions: Comparing Models",
    x = "Date",
    y = "Number of Employees")


