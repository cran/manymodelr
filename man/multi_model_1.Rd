% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_model_1.R
\name{multi_model_1}
\alias{multi_model_1}
\title{Simultaneously train and predict on new data.}
\usage{
multi_model_1(
  old_data,
  yname,
  xname,
  method = NULL,
  metric = NULL,
  control = NULL,
  new_data = NULL,
  ...
)
}
\arguments{
\item{old_data}{The data holding the training dataset}

\item{yname}{The outcome variable}

\item{xname}{The predictor variable(s)}

\item{method}{A vector containing methods to be used as defined in the caret package}

\item{metric}{One of several metrics. Accuracy,RMSE,MAE,etc}

\item{control}{See caret ?trainControl for details.}

\item{new_data}{A data set to validate the model or for which predictions are required}

\item{...}{Other arguments to caret's train function}
}
\value{
A list containing two objects. A tibble containing a summary of the metrics per model,
a tibble containing predicted values and information concerning the model
}
\description{
This function provides a convenient way to train several model types.
It allows a user to predict on new data and depending on the  metrics, the user is able to decide which model
predictions to finally use. The models are built based on Max Kuhn's models in the caret package.
}
\details{
Most of the details of the parameters can be found in the caret package documentation.
This function is meant to help in exploratory analysis to
make an informed choice of the best models
}
\examples{
data("yields", package="manymodelr")
train_set<-createDataPartition(yields$normal,p=0.8,list=FALSE)
valid_set<-yields[-train_set,]
train_set<-yields[train_set,]
ctrl<-trainControl(method="cv",number=5)
set.seed(233)
m<-multi_model_1(train_set,"normal",".",c("knn","rpart"),
"Accuracy",ctrl,new_data =valid_set)
m$Predictions
m$Metrics
m$modelInfo
}
\references{
Kuhn (2014), "Futility Analysis in the Cross-Validation of Machine Learning Models" http://arxiv.org/abs/1405.6974,

Kuhn (2008), "Building Predictive Models in R Using the caret" (http://www.jstatsoft.org/article/view/v028i05/v28i05.pold_data)
}
