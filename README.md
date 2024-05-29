# external_rmodel_example
An example of an external r model to run with CHAP. How to run train and predict using the model is defined in the config.yml file:
```yaml
name: example_model
train_command: 'Rscript train.R {train_data} {model}'
predict_command: 'Rscript predict.R {model} {future_data} {out_file}'
adapters: {'Y': 'disease_cases',
           'E': 'population',
           'T1': 'week',
           'T2': 'year',
           'S1': 'location',
           'rainsum': 'rainfall',
           'meantemperature': 'mean_temperature'}
```
We see from this that the way to train to model is to run the train.R script and provide it the filename of the 
  training data and the filename of where the model should be saved. Further, we see that in order to predict using the model, we run the predict.R script and provide it the filename of the model, 
  the filename of the future data, and the filename of where the predictions should be saved.

The last field specifies how the data should look like. Translating the names of the fields used in the model, to the names used internally in CHAP.
This makes sense when we look at the inla model definition in the train.R file:
```R
formula <- Y ~ 1 + f(T1,  model = "iid") + f(T2, model = "rw1") + rainsum + meantemperature
```
We see that the model uses the name 'rainsum' for what CHAP calls 'rainfall' and 'meantemperature' for what CHAP calls 'mean_temperature'.

In effect, what happens is that CHAP will add fields to the data that are named as the fields used in the model, and then write the data to a file that the model can read.

## train_data
In the example data provided, we see the following fields in the csv file:
```csv
,time_period,rainfall,mean_temperature,disease_cases,population,location,Y,E,T1,T2,S1,rainsum,meantemperature
0,2023-04,27.0,27.0,,533475.0,O6uvpzGd5pu,,200000,17,2023,O6uvpzGd5pu,27.0,27.0
1,2023-05,26.5,26.5,116.0,533475.0,O6uvpzGd5pu,116.0,200000,22,2023,O6uvpzGd5pu,26.5,26.5
2,2023-06,24.9,24.9,139.0,533475.0,O6uvpzGd5pu,139.0,200000,26,2023,O6uvpzGd5pu,24.9,24.9
3,2023-07,24.6,24.6,168.0,533475.0,O6uvpzGd5pu,168.0,200000,31,2023,O6uvpzGd5pu,24.6,24.6
```
The last 7 columns are the ones specifically added for this model, while the first 6 are the ones that are present by default in CHAP.

## future_data
The future_data file is similar to the training data file, but where the disease_cases column is empty. This is the data that the model will use to make predictions.
In the provided example data, the future data looks like this:
```csv
,time_period,rainfall,mean_temperature,population,location,disease_cases,Y,E,T1,T2,S1,rainsum,meantemperature
0,2023-12,26.1,26.1,533475.0,O6uvpzGd5pu,,,200000,52,2023,O6uvpzGd5pu,26.1,26.1
1,2024-01,27.8,27.8,533475.0,O6uvpzGd5pu,,,200000,5,2024,O6uvpzGd5pu,27.8,27.8
2,2024-02,29.2,29.2,533475.0,O6uvpzGd5pu,,,200000,9,2024,O6uvpzGd5pu,29.2,29.2
3,2024-03,29.1,29.1,533475.0,O6uvpzGd5pu,,,200000,13,2024,O6uvpzGd5pu,29.1,29.1
0,2023-12,27.1,27.1,316478.0,PMa2VCrupOd,,,200000,52,2023,PMa2VCrupOd,27.1,27.1
```
We see that the disease_cases column is empty, as well as the Y column, which is the column that the model will predict.

## model
The model file is where we save the state from training to predicting. We see in the train.R file:
```R
args = commandArgs(trailingOnly=TRUE)
# ...
output_model_filename = args[2]
# ...
save(model, file = output_model_filename)
```
While in predict.R we have:
```R
args = commandArgs(trailingOnly=TRUE)
model_filename = args[1] # filename of the saved model
# ...
load(file = model_filename)
```

## out_file
This is where the predictions will be saved. The predictions will usually be in the form of summary statistics for each predicted value. In the example data, the predictions look like this:
```csv
"","X","time_period","rainfall","mean_temperature","population","location","disease_cases","Y","E","T1","T2","S1","rainsum","meantemperature","mean","std","max","min","quantile_low","median","quantile_high"
"1",0,"2023-12",26.1,26.1,533475,"O6uvpzGd5pu",NA,NA,200000,52,2023,"O6uvpzGd5pu",26.1,26.1,92,47.396884353981,244,19,41.8,78,155.1
"2",1,"2024-01",27.8,27.8,533475,"O6uvpzGd5pu",NA,NA,200000,5,2024,"O6uvpzGd5pu",27.8,27.8,87.27,29.0060443021899,175,24,54,83,124.2
"3",2,"2024-02",29.2,29.2,533475,"O6uvpzGd5pu",NA,NA,200000,9,2024,"O6uvpzGd5pu",29.2,29.2,128.18,47.594749731669,294,41,75.8,121,184.3
"4",3,"2024-03",29.1,29.1,533475,"O6uvpzGd5pu",NA,NA,200000,13,2024,"O6uvpzGd5pu",29.1,29.1,143.79,52.3753537149648,341,50,88.5,133.5,210.2
"5",0,"2023-12",27.1,27.1,316478,"PMa2VCrupOd",NA,NA,200000,52,2023,"PMa2VCrupOd",27.1,27.1,59.04,19.6185441031508,136,20,34.8,58.5,81.1
"6",1,"2024-01",28.4,28.4,316478,"PMa2VCrupOd",NA,NA,200000,5,2024,"PMa2VCrupOd",28.4,28.4,111.31,39.6672256135816,232,35,63,105,165.4
"7",2,"2024-02",29.2,29.2,316478,"PMa2VCrupOd",NA,NA,200000,9,2024,"PMa2VCrupOd",29.2,29.2,76.53,28.9595520781254,176,24,45,70.5,113
```
The last 7 columns are the ones that the model outputs and that will be read by CHAP for the evalutaion or visualization.




