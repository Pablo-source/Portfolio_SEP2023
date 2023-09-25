# 2 TS model using PARSNIP

# 2.1 ARIMA model using MODELTIME

pacman::p_load(parsnip,rsample,timetk,modeltime) 

# Check installed pacakges
Mypath<- .libPaths()
(.packages())


# 1. From previous script get the data 
AEATT_data <- AE_data_analysis_prep %>% select(period, type_1_Major_att,type_2_Single_esp_att,type_3_other_att) 
AEATT_data

# 2. Visualize data set. Plot time series
AEATT_plot_Type1 <- AEATT_data %>%  plot_time_series(period, type_1_Major_att)
AEATT_plot_Type2 <- AEATT_data %>%  plot_time_series(period, type_2_Single_esp_att)
AEATT_plot_Type3 <- AEATT_data %>%  plot_time_series(period, type_3_other_att)

# 3. Split Data 80/20
splits <- initial_time_split(AEATT_data, prop = 0.8)



### 4. Create initial train test split
set.seed(20)
data_split <- initial_split(AEATT_data, prop = .8)
data_train <- training(data_split)
data_test <- testing(data_split)


# See my other repo about Modeltime
# Tried with Modeltime (ARIMA, ETS)
#https://cran.r-project.org/web/packages/modeltime/vignettes/getting-started-with-modeltime.html


# 4. Model training
# https://wec.wur.nl/dse/24-tidymodels.html
# As introduced above, the tidymodels package, and here in particular the parsnip package, aims at providing a unified interface to the various machine leaning algorithms offered in R (currently a selection, but this is increasing). So instead of different functions (possibly from different packages) that implements a similar algorithm (e.g. Random Forest, linear regression or Support Vector Machine), parsnip provides a single interface (i.e. function with standardized argument names) for each of the classes of models. See this list of models currently implemented in parsnip.

# Model 1: Linear regression ----

# Let's see whether Type 1 major attendances has some type of linear relationship with Type 2 and time

library(tidymodels)  # Includes the workflows package
tidymodels_prefer()

# Bind modeling and preprocessing objects together

# 1. Define a linear model
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

# 2. A workflow always requires a parsnip model object:

  lm_wflow <- workflow() %>% 
              add_model(lm_model)
  lm_wflow
  
# Notice that we have not yet specified how this workflow should preprocess the data: Preprocessor: None.
# If our model is very simple, a standard R formula can be used as a preprocessor:

# Choose which models to use
show_engines() #  list of possible engines for the model of interest.

# 3. Define pre processor. A standard R formula can be used as a preprocessor:
  
lm_wflow <- 
  lm_wflow %>% 
  add_formula(type_1_Major_att ~ type_2_Single_esp_att + type_3_other_att)

lm_wflow

# 4. Fit model
lm_fit <- fit(lm_wflow, data_train)
lm_fit


#> ══ Workflow ═════════════════════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: linear_reg()
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────────────
#> Sale_Price ~ Longitude + Latitude
#> 
#> ── Model ────────────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm
#> 
#> 

# 4 We can also predict() on the fitted workflow:
predict(lm_fit, data_test %>% slice(1:3))

# 5. Both the model and preprocessor can be removed or updated:

lm_fit %>% update_formula(type_1_Major_att ~ type_2_Single_esp_att)

