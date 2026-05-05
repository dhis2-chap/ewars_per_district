# left side is the names used in the code, right side is the internal names in CHAP
# Cases = number of cases
# E = population
# week = week
# month = month
# ID_year = year
# ID_spat = location
# rainsum = rainfall
# meantemperature = mean_temperature
#note: The model uses either weeks or months

library(yaml)
library(jsonlite)
library(INLA)
library(dlnm)
library(dplyr)
source("lib.R")

#for spatial effects
library(sf)
library(spdep)

parse_model_configuration <- function(file_path) {
  # Load YAML content
  config <- yaml.load_file(file_path)
  print(config)

  # Ensure fields exist and provide defaults if missing
  user_option_values <- if (!is.null(config$user_option_values)) {
    fromJSON(toJSON(config$user_option_values))
  } else {
    list()
  }

  additional_continuous_covariates <- if (!is.null(config$additional_continuous_covariates)) {
    config$additional_continuous_covariates
  } else {
    character()
  }

  # Return the structured list
  list(
    user_option_values = user_option_values,
    additional_continuous_covariates = additional_continuous_covariates
  )
}

generate_model_with_single_lag <- function(df, covariates, nlag) {
  basis_terms <- ""
  covariates <- c(covariates)
  
  for (cov in covariates) {
    var_data <- df[[cov]]
    basis_name <- paste0(cov, "_lag", nlag)
    if (basis_terms == "") {
      basis_terms <- basis_name
    } else {
      basis_terms <- paste(basis_terms, "+", basis_name)
    }
    df <- mutate(df, !!basis_name := dplyr::lag(.data[[cov]], nlag))
  }
  df <- df[(nlag+1):nrow(df),] 
  
  # Generate formula string 
  # formula_str <- paste(
  #   "Cases ~ 1 +",
  #   "f(ID_year, model='iid') +", # just a yearly iid effect
  #   "f(ID_time_cyclic, model='rw1', cyclic=TRUE, scale.model=TRUE) +",
  #   basis_terms
  # )
  
  hyper_iid <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))      # sd(u)=1 with 1% tail
  hyper_rw1 <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))    # a bit tighter on the seasonal smooth
  
  formula_str <- paste("Cases ~ 1 +",
                       "f(ID_year, model = 'iid', hyper = hyper_iid) +",
                       "f(ID_time_cyclic, model = 'rw1', cyclic = TRUE, n = 12, scale.model = TRUE, hyper = hyper_rw1) +", 
                       basis_terms)
  print(formula_str) 
  model_formula <- as.formula(formula_str)
  
  return(list(formula = model_formula, data = df))
}

predict_chap <- function(model_fn, hist_fn, future_fn, preds_fn, config_fn=""){
  #load(file = model_fn) #would normally load a model here
  if (config_fn != "") {
    print("Loading model configuration from YAML file...")
    print(config_fn)
    config <- parse_model_configuration(config_fn)
    covariate_names <- c("rainfall", "mean_temperature") #default values
    # if additional, add these to list:
    if (length(config$additional_continuous_covariates) > 0) {
      covariate_names <- c(covariate_names, config$additional_continuous_covariates)
    }
    nlag<- config$user_option_values$n_lag
    precision <- config$user_option_values$precision
    # Use config$user_option_values and config$additional_continuous_covariates as needed
  } else {
        covariate_names <- c("rainfall", "mean_temperature")
        precision <- 0.01
  }
  future_df <- read.csv(future_fn) #the two columns on the next lines are not normally included in the future df
  future_df$Cases <- rep(NA, nrow(future_df))
  future_df$disease_cases <- rep(NA, nrow(future_df)) #so we can rowbind it with historic
  
  historic_df = read.csv(hist_fn)
  df <- rbind(historic_df, future_df) 
  
  #Standardize climate variables to standard normal, N(0, 1)
  for (covariate in covariate_names){
    df[[covariate]] <- (df[[covariate]] - mean(df[[covariate]])) / sd(df[[covariate]])
  }
  #district <- "Acre"
  
  if( "week" %in% colnames(df)){ # for a weekly model
    df <- mutate(df, ID_time_cyclic = week)
    df <- offset_years_and_weeks(df)
    min_lag <- 4
    max_lag <- 16
  } else{ # for a monthly model
    df <- mutate(df, ID_time_cyclic = month)
    df <- offset_years_and_months(df)
    min_lag <- 1
    max_lag <- 4
  }
  
  df$ID_year <- df$ID_year - min(df$ID_year) + 1 #makes the years 1, 2, ...
  
  unique_districts <- unique(df$ID_spat)
  
  df_with_best_lag_per_dist <- data.frame(district = unique_districts,
    best_lag = NA )
  
  #assumes all districts in future_df has the same number of rows/timepoints
  prediction_period <- nrow(filter(future_df, ID_spat == unique_districts[1]))
  results_df <- data.frame() #an empty placeholder
  district <- "DistritoFederal"
  for (district in unique_districts) {
    print("-----------District-----------------")
    print(district)
    df_dis <- filter(df, ID_spat == district)
    
    #Values to be overwritten
    LS <- 1e7 
    best_lag <- 0 # should be NA or something
    for (lag in min_lag:max_lag) {
      #lag <- 1
      generated <- generate_model_with_single_lag(df_dis, covariate_names, lag)
      test_data <- generated$data

      model <- inla(formula = generated$formula, data = generated$data, family = "nbinomial",
                    offset = log(E), control.inla = list(strategy = 'adaptive'),
                    control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, return.marginals = FALSE),
                    control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1e-4, prec = precision),
                    control.predictor = list(link = 1, compute = TRUE),
                    verbose = T, safe=FALSE)
      summary(model)
      new_LS <- -mean(log(model$cpo$cpo), na.rm = TRUE) #ignores the NA's for the missing cases
      print(new_LS)
      if (new_LS < LS){
        LS <- new_LS
        best_lag <- lag
      }
    }
    generated <- generate_model_with_single_lag(df_dis, covariate_names, best_lag) #best_lag
    
    model <- inla(formula = generated$formula, data = generated$data, family = "nbinomial", 
                  offset = log(E), control.inla = list(strategy = 'adaptive'),
                  control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, return.marginals = FALSE),
                  control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1e-4, prec = precision),
                  control.predictor = list(link = 1, compute = TRUE),
                  verbose = F, safe=FALSE)
    
    summary(model)
    #predictions for the given district
    casestopred <- generated$data$Cases # response variable

    # Predict only for the cases where the response variable is missing AND the
    # row's time_period belongs to future_df. The previous version only checked
    # is.na(Cases), which also matched gap-filled / genuinely missing historic
    # rows; predictions for those rows then ended up in the output CSV under
    # time_periods chap-core's join doesn't expect, leaving the actual forecast
    # horizon NaN after merge (chap-core's pandera schema then rejects with
    # "non-nullable series 'forecast' contains null values").
    future_periods <- unique(future_df$time_period)
    idx.pred <- which(generated$data$time_period %in% future_periods &
                      is.na(casestopred))
    mpred <- length(idx.pred)
    s <- 1000
    y.pred <- matrix(NA, mpred, s)
    # Sample parameters of the model
    xx <- inla.posterior.sample(s, model)  # This samples parameters of the model
    xx.s <- inla.posterior.sample.eval(function(idx.pred) c(theta[1], Predictor[idx.pred]), xx, idx.pred = idx.pred) # This extracts the expected value and hyperparameters from the samples
    
    # Sample predictions
    for (s.idx in 1:s){
      #s.idx <- 1
      xx.sample <- xx.s[, s.idx]
      #print(xx.sample)
      y.pred[, s.idx] <- rnbinom(mpred,  mu = exp(xx.sample[-1]), size = xx.sample[1])
      stopifnot(!is.na(y.pred[, s.idx]))
    }
    
    new.df = data.frame(time_period = generated$data$time_period[idx.pred], location = generated$data$location[idx.pred], y.pred)
    colnames(new.df) = c('time_period', 'location', paste0('sample_', 0:(s-1)))

    # No further truncation needed: idx.pred is already restricted to future_df's
    # time_periods (see the filter above), so new.df contains exactly the rows
    # chap-core's join expects. The previous `best_lag < prediction_period`
    # truncation block was compensating for an over-broad idx.pred that included
    # historic NA rows too — with the filter that compensation now over-crops
    # (e.g. would leave only 1 of 3 future periods for districts with best_lag=1).

    if (nrow(results_df) < 1) {
      results_df <- new.df
    } else {
      results_df <- rbind(results_df, new.df)
    }
    
    #to save the best lags for comparisons between districts
    df_with_best_lag_per_dist$best_lag[df_with_best_lag_per_dist$district == district] <- best_lag
  } #the DIC may be dependent on the chosen lag as it effects the number of datapoints
  # also, higher lags get less data
  #currently predicts for all timepoints in future_df regardless of the chosen best lag, not ideal
  
  # Write new dataframe to file, and save the model?
  write.csv(results_df, preds_fn, row.names = FALSE)
  #saveRDS(model, file = model_fn)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  cat("running predictions")
  print(args)
  model_fn <- args[1]
  hist_fn <- args[2]
  future_fn <- args[3]
  preds_fn <- args[4]
  if (length(args) == 5) {
    config_fn <- args[5]
  } else {
    config_fn <- ""
  }
  predict_chap(model_fn, hist_fn, future_fn, preds_fn, config_fn)
}

#Testing

# model_fn <- "example_data_monthly/model"
# hist_fn <- "example_data_monthly/historic_data.csv"
# future_fn <- "example_data_monthly/future_data.csv"
# preds_fn <- "example_data_monthly/predictions.csv"

