## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  install.packages("manymodelr")
#  

## -----------------------------------------------------------------------------
library(manymodelr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------

head(agg_by_group(iris,.~Species,length))


## -----------------------------------------------------------------------------
head(agg_by_group(mtcars,cyl~hp+vs,sum))

## -----------------------------------------------------------------------------
set.seed(520)
train_set<-createDataPartition(iris$Species,p=0.8,list=FALSE)
valid_set<-iris[-train_set,]
train_set<-iris[train_set,]
ctrl<-trainControl(method="cv",number=5)
m<-multi_model_1(train_set,"Species",".",c("knn","rpart"), "Accuracy",ctrl,new_data =valid_set)


## -----------------------------------------------------------------------------

m$metric


## -----------------------------------------------------------------------------

head(m$predictions)


## -----------------------------------------------------------------------------
# fit a linear model and get predictions
lin_model <- multi_model_2(iris[1:50,],iris[50:99,],"Sepal.Length","Petal.Length","lm")

head(lin_model)

## -----------------------------------------------------------------------------
head(multi_model_2(iris[1:50,],iris[50:99,],"Sepal.Length", "Petal.Length + Sepal.Width","lm"))

## -----------------------------------------------------------------------------
head(multi_model_2(iris[1:50,],iris[50:99,],"Sepal.Length",
    "Petal.Length + I(Sepal.Width)**2","lm"))

## -----------------------------------------------------------------------------
iris1 <- iris[1:50,]
iris2 <- iris[51:100,]
lm_model <- fit_model(iris1,"Sepal.Length","Petal.Length","lm")
lm_model


## -----------------------------------------------------------------------------
extract_model_info(lm_model, "r2")

## -----------------------------------------------------------------------------
extract_model_info(lm_model, "adj_r2")

## -----------------------------------------------------------------------------
extract_model_info(lm_model, "p_value")

## -----------------------------------------------------------------------------
extract_model_info(lm_model,c("p_value","response","call","predictors"))


## -----------------------------------------------------------------------------
# select only column 6 that has our predicted values
head(add_model_predictions(lm_model, old_data = iris1, new_data =  iris2))[6]

## -----------------------------------------------------------------------------
library(dplyr)
iris1 %>% 
  add_model_predictions(model=lm_model,new_data = iris2) %>% 
  select(predicted, everything()) %>% 
  head()

## -----------------------------------------------------------------------------
head(add_model_residuals(lm_model, iris1)[6])

## -----------------------------------------------------------------------------
iris1 %>% 
  add_model_residuals(model=lm_model) %>% 
  add_model_predictions(new_data = iris2, model = lm_model) %>% 
  select(predicted,residuals, everything()) %>% 
  head()

## -----------------------------------------------------------------------------

fit_models(df=iris,yname=c("Sepal.Length","Sepal.Width"),xname="Petal.Length + Petal.Width",modeltype="lm")


## -----------------------------------------------------------------------------

# getall correlations
corrs <- get_var_corr(mtcars,comparison_var="mpg")



## -----------------------------------------------------------------------------


head(corrs)


## -----------------------------------------------------------------------------

# purely demonstrative
get_var_corr(iris,"Sepal.Length",other_vars="Petal.Length",drop_columns=c("factor","character"),method="spearman", exact=FALSE)



## -----------------------------------------------------------------------------

head(get_var_corr_(mtcars),6)


## -----------------------------------------------------------------------------

head(get_var_corr_(mtcars,subset_cols=list(c("mpg","vs"),c("disp","wt")),method="spearman",exact=FALSE))


## -----------------------------------------------------------------------------


plot_corr(mtcars,show_which = "corr",
          round_which = "correlation",decimals = 2,x="other_var",  y="comparison_var",plot_style = "squares"
          ,width = 1.1,custom_cols = c("green","blue","red"),colour_by = "correlation")


## -----------------------------------------------------------------------------
# color by p value
# change custom colors by supplying custom_cols
# significance is default 
set.seed(233)
plot_corr(mtcars, x="other_var", y="comparison_var",plot_style = "circles",show_which = "signif",
          colour_by = "p.value", sample(colours(),3))

## -----------------------------------------------------------------------------
head(rowdiff(iris,exclude = "factor",direction = "reverse"))

## -----------------------------------------------------------------------------
head(na_replace(airquality, how="value", value="Missing"),8)


## -----------------------------------------------------------------------------
test_df <- data.frame(A=c(NA,1,2,3), B=c(1,5,6,NA),groups=c("A","A","B","B"))
# Replace NAs by group
# replcae with the next non NA by group.
na_replace_grouped(df=test_df,group_by_cols = "groups",how="ffill")


