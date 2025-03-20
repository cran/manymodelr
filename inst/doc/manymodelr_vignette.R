## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# 
# install.packages("manymodelr")
# 

## -----------------------------------------------------------------------------

library(manymodelr)
data("yields", package="manymodelr")


## -----------------------------------------------------------------------------
set.seed(520)
train_set<-createDataPartition(yields$normal,p=0.6,list=FALSE)
valid_set<-yields[-train_set,]
train_set<-yields[train_set,]
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

models<-fit_models(df=yields,yname=c("height", "weight"),xname="yield",
                   modeltype="glm") 


## -----------------------------------------------------------------------------


res_residuals <- lapply(models[[1]], add_model_residuals,yields)
res_predictions <- lapply(models[[1]], add_model_predictions, yields, yields)
# Get height predictions for the model height ~ yield 
head(res_predictions[[1]])


## -----------------------------------------------------------------------------
fit_models(df=yields,yname=c("height","weight"),
           xname=".",modeltype=c("lm","glm"), drop_non_numeric = TRUE)


## -----------------------------------------------------------------------------

extract_model_info(lm_model, "r2")


## -----------------------------------------------------------------------------

extract_model_info(lm_model, "adj_r2")


## -----------------------------------------------------------------------------

extract_model_info(lm_model, "p_value")


## -----------------------------------------------------------------------------

extract_model_info(lm_model,c("p_value","response","call","predictors"))


## -----------------------------------------------------------------------------

# getall correlations

# default pearson

head( corrs <- get_var_corr(mtcars,comparison_var="mpg") )



## -----------------------------------------------------------------------------

# purely demonstrative
get_var_corr(yields,"height",other_vars="weight",
             drop_columns=c("factor","character"),method="spearman",
             exact=FALSE)



## -----------------------------------------------------------------------------

head(get_var_corr_(yields),6)


## -----------------------------------------------------------------------------

head(get_var_corr_(mtcars,subset_cols=list(c("mpg","vs"),c("disp","wt")),
                   method="spearman",exact=FALSE))


## -----------------------------------------------------------------------------


plot_corr(mtcars,show_which = "corr",
          round_which = "correlation",decimals = 2,x="other_var",  y="comparison_var",plot_style = "squares"
          ,width = 1.1,custom_cols = c("green","blue","red"),colour_by = "correlation")


## -----------------------------------------------------------------------------
# color by p value
# change custom colors by supplying custom_cols
# significance is default 
set.seed(233)
plot_corr(mtcars, x="other_var", y="comparison_var",plot_style = "circles",show_which = "signif", colour_by = "p.value", sample(colours(),3))


## -----------------------------------------------------------------------------

head(agg_by_group(yields,.~normal,length))


## -----------------------------------------------------------------------------

head(agg_by_group(mtcars,cyl~hp+vs,sum))


## -----------------------------------------------------------------------------

head(rowdiff(yields,exclude = "factor",direction = "reverse"))


## -----------------------------------------------------------------------------

head(na_replace(airquality, how="value", value="Missing"),8)


## -----------------------------------------------------------------------------
test_df <- data.frame(A=c(NA,1,2,3), B=c(1,5,6,NA),groups=c("A","A","B","B"))
# Replace NAs by group
# replace with the next non NA by group.
na_replace_grouped(df=test_df,group_by_cols = "groups",how="ffill")


