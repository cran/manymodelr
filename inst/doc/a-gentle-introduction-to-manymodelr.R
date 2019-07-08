## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
#install.packages("manymodelr")

## ------------------------------------------------------------------------
library(manymodelr)

## ------------------------------------------------------------------------
agg_by_group(iris,.~Species,length)

## ------------------------------------------------------------------------
head(agg_by_group(mtcars,cyl~hp+vs,sum))

## ------------------------------------------------------------------------
suppressMessages(library(caret))
set.seed(520)
train_set<-createDataPartition(iris$Species,p=0.8,list=FALSE)
valid_set<-iris[-train_set,]
train_set<-iris[train_set,]
ctrl<-trainControl(method="cv",number=5)
m<-multi_model_1(train_set,"Species",".",c("knn","rpart"),
"Accuracy",ctrl,newdata =valid_set,valid=TRUE)

## ------------------------------------------------------------------------
m$Metrics

## ------------------------------------------------------------------------
head(m$Predictions)

## ------------------------------------------------------------------------
iris1<-iris[1:60,]
iris2<-iris[60:nrow(iris),]
m1<-modeleR(iris1,Sepal.Length,Petal.Length,
        lm,na.rm=TRUE,iris2)
head(m1$Predictions)

## ------------------------------------------------------------------------
get_var_corr(mtcars, "mpg",get_all = TRUE)

## ------------------------------------------------------------------------
get_var_corr(mtcars,comparison_var = "cyl",
             other_vars = c("disp","mpg"),get_all = FALSE)

## ------------------------------------------------------------------------
head(get_var_corr_(mtcars),6)

## ------------------------------------------------------------------------
head(rowdiff(iris,exclude = "non_numeric",direction = "reverse"))

