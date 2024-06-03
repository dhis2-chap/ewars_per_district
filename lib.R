local({r <- getOption("repos")
       r["CRAN"] <- "https://cran.r-project.org"
       options(repos=r)
})
install.packages(c("tsModel", "dlnm"))

library(tsModel)
library(dlnm)

get_crossbasis <- function(var, group, nlag){
    tsModel::Lag(var, group = group, k = 0:nlag)
    lagknot = equalknots(0:nlag, 2)
    basis <- crossbasis(var, argvar = list(fun = "ns", knots = equalknots(var, 2)), arglag = list(fun = "ns", knots = nlag/2))
}


extra_fields <- function(df) {
    basis_meantemperature <- get_crossbasis(df$meantemperature, df$S1, 6)
    colnames(basis_meantemperature) = paste0("basis_meantemperature.", colnames(basis_meantemperature))
    print(basis_meantemperature)
    return (basis_meantemperature)
}

mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
{
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'),
                control.compute = list(dic = TRUE, config = config, cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE),
                verbose = F, safe=FALSE)
  return(model)
}


formula <- Y ~ 1 + f(T1,  model = "rw1", cyclic = TRUE, scale.model=TRUE) + f(T2, model = "rw1") + rainsum + basis_meantemperature