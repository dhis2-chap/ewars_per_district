
source("train.R")
source("predict.R")

# #test for weekly data, fails with nlag = 12 since the dataset is to small, works with 5 for example
# train_chap("example_data/trainData.csv", "example_data/model")
# predict_chap("example_data/model", "example_data/training_data.csv", "example_data/future_data.csv", "example_data/predictions.csv")

#test for monthly data
train_chap("example_data_monthly/trainData.csv", "example_data_monthly/model")
predict_chap("example_data_monthly/model", "example_data_monthly/historic_data.csv", "example_data_monthly/future_data.csv", "example_data_monthly/predictions.csv")

#testing with the data and config from the failed run from chap-core cli
df_dis <- filter(historic_df, ID_spat == "Acre")
predict_chap("example_data_monthly/model", "historic_data.csv", "future_data.csv", "example_data_monthly/predictions.csv", "model_configuration_for_run.yaml")

# library(tsibble)
# library(dplyr)
# preds <- read.csv("example_data/predictions.csv")
# model <- readRDS("example_data/model")
# 
# summary(model)
# 
# preds <- filter(preds, yearmonth(time_period) >= yearmonth("2017-01")) #only works for this specific test data
# yearmonth(preds[1, "time_period"]) < yearmonth("2017-01")

hei <- packageVersion("INLA")
packageDescription("INLA")$Version

update.packages(ask = FALSE, checkBuilt = TRUE)

install.packages("remotes")
library(remotes)
remotes::install_version("INLA", version = "25.06.13",
                         repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/testing"), dep = TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

rows_with_na <- df[apply(historic_df, 1, function(x) any(is.na(x))), ]
