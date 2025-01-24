# external_rmodel_example
How to run train and predict using the model is defined in the yaml file MLproject: (MIGHT NEED TO UPDATE THIS FOR PLOYGONS ETC)
```yaml
name: EWARS

adapters: {'Cases': 'disease_cases',
           'E': 'population',
           'week': 'week',
           'ID_year': 'year',
           'ID_spat': 'location',
           'rainsum': 'rainfall',
           'meantemperature': 'mean_temperature'}

docker_env:
  image: ivargr/r_inla:latest

entry_points:
  train:
    parameters:
      train_data: path
      model: str
    command: "Rscript train.R {train_data} {model}"
  predict:
    parameters:
      historic_data: path
      future_data: path
      model: str
      out_file: path
    command: "Rscript predict.R {model} {historic_data} {future_data} {out_file} None samples"
```
The first part, the adapter, chages the variable names internaly in CHAP to whatever names we want to use in our code. The internal names are on the right side. To train the model is done through the Rscript command, and we supplu the filename for the training data a filename for saving the model.
 Further, we see that in order to predict using the model, we run the predict.R script and provide it the filename of the model, the filename of the historic data, the filename of the future data, and the filename of where the predictions should be saved. There are also two additional arguments which are not used at the moment. (THIS MIGHT CHANGE)

The use of the adapter becomes clearer when viewing the formula in the predict.R (SHOULD BE TRAIN.R IDEALLY)
```
lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid') + 
      f(month_num, model = "rw1", scale.model = T, replicate = ID_spat_num) +
      f(month, model='rw1', cyclic=T, scale.model=T) + basis_meantemperature + basis_rainsum
```
We see that the model uses the name 'Cases' for what CHAP calls 'disease_cases' and 'ID_spat' for what CHAP calls 'location'. There are also aditional column names that do not correpsond with either naming conventions, and these are new columns created in the Rscripts, for example 'month_num'.

In effect, what happens is that CHAP will add fields to the data that are named as the fields used in the model, and then write the data to a file that the model can read. (MIGHT ALSO CHANGE, UNNECESSARY LARGE DATASET, BUT NOT THAT IMPORTANT)

## train_data
In the example data provided, we see the following fields in the csv file:
```csv
"time_period","rainfall","mean_temperature","disease_cases","population","location","Cases","E","week","ID_year","ID_spat","rainsum","meantemperature"
"2023-05",26.5,26.5,116,533475,"O6uvpzGd5pu",116,200000,22,2023,"O6uvpzGd5pu",26.5,26.5
"2023-06",24.9,24.9,139,533475,"O6uvpzGd5pu",139,200000,26,2023,"O6uvpzGd5pu",24.9,24.9
"2023-07",24.6,24.6,168,533475,"O6uvpzGd5pu",168,200000,31,2023,"O6uvpzGd5pu",24.6,24.6
"2023-08",24.5,24.5,71,533475,"O6uvpzGd5pu",71,200000,35,2023,"O6uvpzGd5pu",24.5,24.5
"2023-09",24.7,24.7,141,533475,"O6uvpzGd5pu",141,200000,39,2023,"O6uvpzGd5pu",24.7,24.7
```
The last 7 columns are the ones specifically added for this model, while the first 6 are the ones that are present by default in CHAP.

## future_data
The future_data file is similar to the training data file, but where the disease_cases column is empty. This is the data that the model will use to make predictions.
In the provided example data, the future data looks like this:
```csv
"time_period","rainfall","mean_temperature","population","location","disease_cases","Cases","E","week","ID_year","ID_spat","rainsum","meantemperature"
"2023-12",26.1,26.1,533475,"O6uvpzGd5pu",NA,NA,200000,52,2023,"O6uvpzGd5pu",26.1,26.1
"2024-01",27.8,27.8,533475,"O6uvpzGd5pu",NA,NA,200000,5,2024,"O6uvpzGd5pu",27.8,27.8
"2024-02",29.2,29.2,533475,"O6uvpzGd5pu",NA,NA,200000,9,2024,"O6uvpzGd5pu",29.2,29.2
"2024-03",29.1,29.1,533475,"O6uvpzGd5pu",NA,NA,200000,13,2024,"O6uvpzGd5pu",29.1,29.1
"2023-12",27.1,27.1,316478,"PMa2VCrupOd",NA,NA,200000,52,2023,"PMa2VCrupOd",27.1,27.1
```
We see that the disease_cases column is NA, as well as the Cases column, which is the column that the model will predict.

## model
(THIS IS CURRENTLY NOT USED)
The model file is where we save the state from training to predicting. We see in the train.R file:
```R
args = commandArgs(trailingOnly=TRUE)
# ...
model_fn = args[2]
# ...
save(model, file = model_fn)
```
While in predict.R we have:
```R
args = commandArgs(trailingOnly=TRUE)
model_fn = args[1] # filename of the saved model
# ...
load(file = model_fn)
```

## out_file
This is where the predictions will be saved. The predictions will usually be in the form of many simulated samples for each data point. In the example data, the predictions look like this:
```csv
"time_period","location","sample_0","sample_1","sample_2","sample_3","sample_4","sample_5", ...
"2023-12","O6uvpzGd5pu",88,77,83,67,62, ...
2024-01","O6uvpzGd5pu",87,164,62,70,25, ...
"2024-02","O6uvpzGd5pu",98,113,42,43,22, ...
```
This just shows the first 5 of the 1000 samples for each datapoint saved by CHAP.

## Explaining the code
For now everything happens in the predict.R file, so that will be the focus. We first call the necessary libraries and source some helper functions from lib.R. Then in predict_chap we rowbind the historic and future data together. The next step depends on whether the data is weekly or monthly and assigns different values for the lag and defines ID_time_cyclic, also offsets the years and weeks/months (NOT NEEDED ANYMORE). We then make a index for each location for the number of weeks or months since the first observation. The function crossbasis from dlnm is then used to fit splines for both 'rainsum' and 'meantemperature', and we are finaly ready to define the model formula.
```R
lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid') + 
    f(ID_time, model = "rw1", scale.model = T) +
    f(ID_time_cyclic, model='rw1', cyclic=T, scale.model=T) + basis_meantemperature + basis_rainsum

  model <- inla(formula = lagged_formula, data = df, family = "nbinomial", offset = log(E),
                control.inla = list(strategy = 'adaptive'),
                control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE),
                verbose = F, safe=FALSE)
```
The formula defines the relationship between the different columns in the dataframe and the exogenous variables, which is then used by the inla call to fit the model. We also choose the 'nbinomial' family and an offset. The code after the model call samples from the fit model and writes the final dataframe to the the filelocation in 'preds_fn'. The code after the function ends is used when running the Rscript command, either by CHAP or from your own command line and assigns the arguments to the correct variables and calls predict_chap.   

