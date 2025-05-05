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


library(INLA)
library(dlnm)
library(dplyr)
source("lib.R")

#for spatial effects
library(sf)
library(spdep)

predict_chap <- function(model_fn, hist_fn, future_fn, preds_fn){
  #load(file = model_fn) #would normally load a model here
  
  df <- read.csv(future_fn) #the two columns on the next lines are not normally included in the future df
  df$Cases <- rep(NA, nrow(df))
  df$disease_cases <- rep(NA, nrow(df)) #so we can rowbind it with historic
  
  historic_df = read.csv(hist_fn)
  df <- rbind(historic_df, df) 
  
  if( "week" %in% colnames(df)){ # for a weekly model
    nlag <- 12
    df <- mutate(df, ID_time_cyclic = week)
    df <- offset_years_and_weeks(df)
  } else{ # for a monthly model
    nlag <- 3
    df <- mutate(df, ID_time_cyclic = month)
    df <- offset_years_and_months(df)
  }
  
  df$ID_year <- df$ID_year - min(df$ID_year) + 1 #makes the years 1, 2, ...
  
  basis_meantemperature <- crossbasis(df$meantemperature, lag=nlag, 
                                      argvar = list(fun = "ns", knots = equalknots(df$meantemperature, 2)), 
                                      arglag = list(fun = "ns", knots = nlag/2), group = df$ID_spat)
  colnames(basis_meantemperature) = paste0("basis_meantemperature.", colnames(basis_meantemperature))
  
  basis_rainsum <- crossbasis(df$rainsum, lag=nlag, 
                              argvar = list(fun = "ns", knots = equalknots(df$rainsum, 2)), 
                              arglag = list(fun = "ns", knots = nlag/2), group = df$ID_spat)
  colnames(basis_rainsum) = paste0("basis_rainsum.", colnames(basis_rainsum))
  
  lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid', replicate=ID_year) + 
    f(ID_time_cyclic, model='rw1', cyclic=T, scale.model=T) + 
    basis_meantemperature + basis_rainsum
  
  # if(graph_fn != ""){
  #   df$ID_spat_num2 <- df$ID_spat_num
  #   
  #   geojson <- st_read(graph_fn)
  #   geojson <- st_make_valid(geojson) #some geojson files will giver errors later if this is not cleaned here
  #   
  #   df_locs <- unique(df$location)
  #   geojson_red <- geojson[geojson$VARNAME_1 %in% df_locs, ] #only keeps the regions present in the dataframe
  #   #both df_locs and geosjon_red should be in alfabetical order, believe the files are alfabetical in CHAP, might need to sort them
  #   #if they are both alphabetical, and take care about which field in the geeojson, could be vietnamese or english (handle in CHAP?)
  #   #then the regions should harmonize correctly in the formulas below
  #   
  #   nb <- poly2nb(geojson_red, queen = TRUE) #adjacent polygons are neighbors
  #   adjacency <- nb2mat(nb, style = "B", zero.policy = TRUE) #converts it to an adjacency matrix which is passed to the bym2
  #   
  #   lagged_formula <- Cases ~ 1 + f(ID_spat_num, model = "bym2", graph = adjacency) +
  #     f(ID_time_cyclic, model='rw1', cyclic=T, scale.model=T) + 
  #     basis_meantemperature + basis_rainsum
  # }

  model <- inla(formula = lagged_formula, data = df, family = "nbinomial", offset = log(E),
                control.inla = list(strategy = 'adaptive'),
                control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE),
                verbose = F, safe=FALSE)
  
  casestopred <- df$Cases # response variable
  
  # Predict only for the cases where the response variable is missing
  idx.pred <- which(is.na(casestopred)) #this then also predicts for historic values that are NA, not ideal
  mpred <- length(idx.pred)
  s <- 1000
  y.pred <- matrix(NA, mpred, s)
  # Sample parameters of the model
  xx <- inla.posterior.sample(s, model)  # This samples parameters of the model
  xx.s <- inla.posterior.sample.eval(function(idx.pred) c(theta[1], Predictor[idx.pred]), xx, idx.pred = idx.pred) # This extracts the expected value and hyperparameters from the samples
  
  # Sample predictions
  for (s.idx in 1:s){
    xx.sample <- xx.s[, s.idx]
    y.pred[, s.idx] <- rnbinom(mpred,  mu = exp(xx.sample[-1]), size = xx.sample[1])
  }
  
  # make a dataframe where first column is the time points, second column is the location, rest is the samples
  # rest of columns should be called sample_0, sample_1, etc
  new.df = data.frame(time_period = df$time_period[idx.pred], location = df$location[idx.pred], y.pred)
  colnames(new.df) = c('time_period', 'location', paste0('sample_', 0:(s-1)))
  
  # Write new dataframe to file, and save the model?
  write.csv(new.df, preds_fn, row.names = FALSE)
  saveRDS(model, file = model_fn)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  cat("running predictions")
  model_fn <- args[1]
  hist_fn <- args[2]
  future_fn <- args[3]
  preds_fn <- args[4]
  #graph_fn <- args[5]
  
  predict_chap(model_fn, hist_fn, future_fn, preds_fn)#, graph_fn)
}

#Testing

# model_fn <- "example_data_monthly/model"
# hist_fn <- "example_data_monthly/historic_data.csv"
# future_fn <- "example_data_monthly/future_data.csv"
# preds_fn <- "example_data_monthly/predictions.csv"

