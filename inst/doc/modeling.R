## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(manymodelr)
set.seed(520)
# Create a simple dataset with a binary target
# Here normal is a fictional target where we assume that it meets 
# some criterion means 
sample_data <- data.frame(normal = as.factor(rep(c("Yes", "No"), 500)), 
                          height=rnorm(100, mean=0.5, sd=0.2), 
                          weight=runif(100,0, 0.6),
                          yield = rnorm(100, mean =520, sd = 10))

head(sample_data)

## -----------------------------------------------------------------------------
set.seed(520)
train_set<-createDataPartition(sample_data$normal,p=0.6,list=FALSE)
valid_set<-sample_data[-train_set,]
train_set<-sample_data[train_set,]
ctrl<-trainControl(method="cv",number=5)
m<-multi_model_1(train_set,"normal",".",c("knn","rpart"), 
                 "Accuracy",ctrl,new_data =valid_set)


## -----------------------------------------------------------------------------

m$metric


## -----------------------------------------------------------------------------

head(m$predictions)


## -----------------------------------------------------------------------------
# fit a linear model and get predictions
lin_model <- multi_model_2(mtcars[1:16,],mtcars[17:32,],"mpg","wt","lm")

lin_model[c("predicted", "mpg")]


## -----------------------------------------------------------------------------

multi_lin <- multi_model_2(mtcars[1:16, ], mtcars[17:32,],"mpg", "wt + disp + drat","lm")

multi_lin[,c("predicted", "mpg")]


## -----------------------------------------------------------------------------
lm_model <- fit_model(mtcars,"mpg","wt","lm")
lm_model


## -----------------------------------------------------------------------------

models<-fit_models(df=sample_data,yname=c("height", "weight"),xname="yield",
                   modeltype="glm") 


## -----------------------------------------------------------------------------


res_residuals <- lapply(models[[1]], add_model_residuals,sample_data)
res_predictions <- lapply(models[[1]], add_model_predictions, sample_data, sample_data)
# Get height predictions for the model height ~ yield 
head(res_predictions[[1]])


## -----------------------------------------------------------------------------
fit_models(df=sample_data,yname=c("height","weight"),
           xname=".",modeltype=c("lm","glm"), drop_non_numeric = TRUE)


## -----------------------------------------------------------------------------

extract_model_info(lm_model, "r2")


## -----------------------------------------------------------------------------

extract_model_info(lm_model, "adj_r2")


## -----------------------------------------------------------------------------

extract_model_info(lm_model, "p_value")


## -----------------------------------------------------------------------------

extract_model_info(lm_model,c("p_value","response","call","predictors"))


