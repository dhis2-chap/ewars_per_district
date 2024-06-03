# Y = number of cases
# E = pop.var.dat
# T1 = week
# T2 = year
# S1 = district
r = getOption("repos"); r["CRAN"] = "http://cran.us.r-project.org"; options(repos = r); install.packages(c("tsModel", "dlnm"), repos=c(getOption("repos"), dep=TRUE))
library(INLA)
source('lib.R') # mymodel, extra_fields
# Read in command line args filenames
args = commandArgs(trailingOnly=TRUE)
data_filename = args[1]
output_model_filename = args[2]


df = read.table(data_filename, sep=',', header=TRUE)
basis_meantemperature = extra_fields(df)

# Train the model
model = mymodel(formula, df, config = TRUE)

save(model, file = output_model_filename)
