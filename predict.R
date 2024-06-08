# !/usr/bin/env Rscript
# Y = number of cases
# E = pop.var.dat
# T1 = week
# T2 = year
# S1 = district

library(INLA)

args = commandArgs(trailingOnly=TRUE)
model_filename = args[1] # filename of the saved model
data_filename =  args[2] # filename of the data necessary for prediction
out_filename =  args[3] # where to save the predictions
graph_filename =  args[4] # filename of the graph
#data_filename = 'future_data.csv'
#model_filename = 'tmp.csv'
#out_filename = 'tmp.txt'
#graph_filename = 'none'
source('lib.R') # mymodel, extra_fields
#inla.debug.graph(graph_filename)
# Load the model
load(file = model_filename)

# Load the data
df <- read.table(data_filename, sep=',', header=TRUE)
df$week = as.numeric(substr(df$time_period, 6, 8))
basis_meantemperature = extra_fields(df)

# create a row mask for any missing values in row
na.mask = apply(basis_meantemperature, 1, function(row) (any(is.na(row))))
df = df[!na.mask,]
basis_meantemperature = basis_meantemperature[!na.mask,]
model = mymodel(selectedFormula, df, config = TRUE)
#model2 = mymodel(lagged_formula, df, config = TRUE)


#For each top 5
## Do out of sample
## Set NA one year, train model, sample from the posterior of  the NA, calculate |Prediction-Truth|
##
## Calculate MAE

casestopred <- df$Cases # response variable

# Predict only for the cases where the response variable is missing
idx.pred <- which(is.na(casestopred))
mpred <- length(idx.pred)
s <- 100
y.pred <- matrix(NA, mpred, s)
    # Sample parameters of the model
xx <- inla.posterior.sample(s, model)  # This samples parameters of the model
xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx) # This extracts the expected value and hyperparameters from the samples

# Sample predictions
for (s.idx in 1:s){
    xx.sample <- xx.s[, s.idx]
    y.pred[, s.idx] <- rnbinom(mpred,  mu = exp(xx.sample[-1]), size = xx.sample[1])
    }
#print(y.pred)
# Generate new dataframe with summary statistics
#print(y.pred)
new.df = df[idx.pred,]
new.df$mean = rowMeans(y.pred)
new.df$std = apply(y.pred, 1, sd)
new.df$max = apply(y.pred, 1, max)
new.df$min = apply(y.pred, 1, min)
new.df$quantile_low = apply(y.pred, 1, function(row) quantile(row, 0.1))
new.df$median = apply(y.pred, 1, function(row) quantile(row, 0.5))
new.df$quantile_high = apply(y.pred, 1, function(row) quantile(row, 0.9))

# Write new dataframe to file
write.csv(new.df, out_filename)
