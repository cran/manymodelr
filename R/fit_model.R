#' @param df A data.frame object
#' @inheritParams multi_model_1
#' @param modeltype A character specifying the model type e.g lm for 
#' linear model
#' @param drop_non_numeric Should non numeric columns be dropped? 
#' Defaults to FALSE
#' @param ... Other arguments to specific model types.
#' @name fit_model
#' @title Fit and predict in a single function.
#' @examples
#' fit_model(iris,"Sepal.Length","Species","lm")
#' fit_model(iris,"Sepal.Width",
#' "Sepal.Length + Petal.Length + I(Petal.Width)**2","lm")

#' @export
fit_model <- function (df=NULL, yname=NULL, xname=NULL, modeltype=NULL,
                       drop_non_numeric=FALSE,...){
if(any(is.null(df), is.null(yname),is.null(xname),
       is.null(modeltype))) stop("All arguments must be supplied.")
  
  use_df <- df

if(drop_non_numeric) use_df <- drop_non_numeric(df)
  
model_formula <- as.formula(paste(yname, "~", xname))
do.call(modeltype, list(data = quote(use_df), model_formula,...))
}


#' @export
#' @title Fit several models with different response variables
#' @inheritParams fit_model
#' @return A list of model objects that can be used later.
#' @examples 
#' fit_models(df=iris,yname=c("Sepal.Length","Sepal.Width"),
#' xname="Petal.Length + Petal.Width",modeltype="lm")
#' #many model types
#' fit_models(df=iris,yname=c("Sepal.Length","Sepal.Width"), 
#' xname="Petal.Length + Petal.Width",modeltype=c("lm","glm"))
fit_models <- function (df=NULL,yname=NULL, xname=NULL, modeltype=NULL,
                        drop_non_numeric=FALSE,
                        ...){

if(!length(yname)>1){

stop("fit_models is used for several yname, use fit_model for single predictors")
}
lapply(seq_along(modeltype), function(x) lapply(yname,function(y) 
  fit_model(df=df,yname=y,xname=xname,modeltype = modeltype[x],
            drop_non_numeric = drop_non_numeric, ...)))
  


}




