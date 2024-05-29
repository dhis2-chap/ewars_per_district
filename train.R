# Y = number of cases
# E = pop.var.dat
# T1 = week
# T2 = year
# S1 = district
library(INLA)
# Read in command line args filenames
args = commandArgs(trailingOnly=TRUE)
data_filename = args[1]
output_model_filename = args[2]

mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
{
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'),
                control.compute = list(dic = TRUE, config = config, cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE),
                verbose = T, safe=FALSE)
  return(model)
}

# Define the formula
formula <- Y ~ 1 + f(T1,  model = "iid") + f(T2, model = "rw1") + rainsum + meantemperature

df = read.table(data_filename, sep=',', header=TRUE)

# Train the model
model = mymodel(formula, df, config = TRUE)

save(model, file = output_model_filename)
