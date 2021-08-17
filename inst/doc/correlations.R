## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(manymodelr)
# getall correlations

# default pearson

head( corrs <- get_var_corr(mtcars,comparison_var="mpg") )



## -----------------------------------------------------------------------------
data("yields", package="manymodelr")
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


