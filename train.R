# Y = number of cases
# E = pop.var.dat
# T1 = week
# T2 = year
# S1 = district

# library(INLA)
#
# # Read in command line args filenames
# args = commandArgs(trailingOnly=TRUE)
# data_filename = args[1]
# output_model_filename = args[2]
# graph_filename = args[3]
# source('lib.R') # mymodel, extra_fields
# print(graph_filename)
# #inla.debug.graph(graph_filename)
# df = read.table(data_filename, sep=',', header=TRUE)
# basis_meantemperature = extra_fields(df)
#
# Train the model
#model = mymodel(formula, df, config = TRUE)

#save(model, file = output_model_filename)
