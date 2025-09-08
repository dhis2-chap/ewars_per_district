# ewars_per_district for weekly and monthly data

This model trains on the districts individually and finds the most suitably lag for the climate covariates through optimizing logarithmic scoring (LS). A consequence of this is different prediciton horizons for each district, which could be troubelsome in a production enviroment. It currently uses rainfall and mean temperature as covariates, but others could easily be added. The model combines the covariates with the chosen lag with random effects to capture patterns which are unaccounted for. 

This model is currently not finished and is a work in progress.


## The difference between weekly and monthly
```R
if( "week" %in% colnames(df)){ # for a weekly model
           nlag <- 12
           df <- mutate(df, ID_time_cyclic = week)
           df <- offset_years_and_weeks(df)
} else{ # for a monthly model
           nlag <- 3
           df <- mutate(df, ID_time_cyclic = month)
           df <- offset_years_and_months(df)
}
```
The above code shows the difference needed between weekly and monthly data. We assume weekly input data will have a `week` column and we then define variables for the different cases. For instance weeks have `nlag = 12` while months have `nlag = 3`, which correpsonds to roughly the same amount of time. We also consrtuct the `ID_time_cyclic` column from either weeks or months so the formula later is consistent in both cases.  

