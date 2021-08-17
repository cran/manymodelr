## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(manymodelr)

## -----------------------------------------------------------------------------
# Load the yields dataset
data("yields")
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


