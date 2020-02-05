## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#install.packages("manymodelr")

## -----------------------------------------------------------------------------
library(manymodelr)

## -----------------------------------------------------------------------------
agg_by_group(iris,.~Species,length)

## -----------------------------------------------------------------------------
head(agg_by_group(mtcars,cyl~hp+vs,sum))

## -----------------------------------------------------------------------------
suppressMessages(library(caret))
set.seed(520)
train_set<-createDataPartition(iris$Species,p=0.8,list=FALSE)
valid_set<-iris[-train_set,]
train_set<-iris[train_set,]
ctrl<-trainControl(method="cv",number=5)
m<-multi_model_1(train_set,"Species",".",c("knn","rpart"),
"Accuracy",ctrl,newdata =valid_set,valid=TRUE)

## -----------------------------------------------------------------------------
m$Metrics

## -----------------------------------------------------------------------------
head(m$Predictions)

## -----------------------------------------------------------------------------
# fit a linear model and get predictions
lin_model <- multi_model_2(iris[1:50,],iris[50:99,],"Sepal.Length","Petal.Length","lm")

head(lin_model)

## -----------------------------------------------------------------------------
head(multi_model_2(iris[1:50,],iris[50:99,],"Sepal.Length",
    "Petal.Length + Sepal.Width","lm"))

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
extract_model_info(lm_model,c("p_value","response"))


## -----------------------------------------------------------------------------
# select only column 6 that has our predicted values
head(add_model_predictions(lm_model, old_data = iris1, 
                           new_data =  iris2))[6]

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

# getall correlations
corrs <- get_var_corr(mtcars,comparison_var="mpg")



## -----------------------------------------------------------------------------


head(corrs)


## -----------------------------------------------------------------------------

# purely demonstrative
get_var_corr(iris,"Sepal.Length",other_vars="Petal.Length",drop_columns= c("factor","character"), method="spearman", exact=FALSE)



## -----------------------------------------------------------------------------
head(get_var_corr_(mtcars),6)


## -----------------------------------------------------------------------------
get_var_corr_(mtcars,
             subset_df = TRUE,
             subset_cols = list(c("mpg","vs"),
                                c("disp","wt")),
             method="spearman",exact=FALSE)

## -----------------------------------------------------------------------------
# Use defaults.
#res <- get_var_corr_(mtcars)
#plot_corr(res)

## -----------------------------------------------------------------------------
#plot_corr(res, x="Other_Var", y="Comparison_Var")

## -----------------------------------------------------------------------------
# color by p value
# change custom colors by supplying custom_cols
# significance is default 
#plot_corr(res, x="Other_Var", y="Comparison_Var",
 #         plot_style = "squares",
  #        show_corr = FALSE, show_signif = TRUE,
   #       colour_by = "p.value",
    #      custom_cols = c("blue","yellow","red"))

## -----------------------------------------------------------------------------
#plot_corr(res, x="Other_Var", y="Comparison_Var",
 #         plot_style = "squares",
  #        show_corr = FALSE, show_signif = TRUE,
   #       colour_by = "p.value",
    #      custom_cols = c("blue","yellow","red"),
     #     signif_cutoff = 0.01)

## -----------------------------------------------------------------------------
head(rowdiff(iris,exclude = "factor",direction = "reverse"))

## -----------------------------------------------------------------------------
head(na_replace(airquality, how="value", value="Missing"),8)

## -----------------------------------------------------------------------------
test_df <- data.frame(A=c(NA,1,2,3), B=c(1,5,6,NA),
                      groups=c("A","A","B","B"))
# Replace NAs by group
#agg_by_group(test_df,.~groups,mean)
# from the above we see means are [1,5] [2,6]
na_replace_grouped(df=test_df,group_by_cols = "groups",
                   how="mean")

