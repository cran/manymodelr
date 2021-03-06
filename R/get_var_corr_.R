#' Get correlations for combinations
#' @importFrom utils combn
#' @param df A `data.frame` object for which correlations are required in combinations.
#' @param subset_cols A `list` of length 2. The values in the list correspond to the comparison
#' and other_Var arguments in `get_var_corr`. See examples below.
#' @param ... Other arguments to `get_var_corr`
#' @inheritParams get_var_corr
#' @return A data.frame object with combinations.
#' @details This function extends get_var_corr by providing an opportunity to get correlations
#' for combinations of variables. It is currently slow and may take up to a minute depending on system specifications.
#' @examples 
#' get_var_corr_(mtcars,method="pearson")
#' #use only a subset of the data.
#'  get_var_corr_(mtcars,
#'              subset_cols = list(c("mpg","vs"),
#'                                 c("disp","wt")),
#'              method="spearman",exact=FALSE)
#' @export
get_var_corr_<-function(df,subset_cols=NULL,
                        drop_columns = c("character","factor"),
                        ...){
  UseMethod("get_var_corr_")
}
#' @export
get_var_corr_.data.frame<-function(df,subset_cols=NULL,
                                   drop_columns = c("character","factor"),
                                  ...){

if(any(sapply(df,class) %in% drop_columns)){
 df<- Filter(function(x) ! class(x) %in% drop_columns,df)
 warning("Columns with classes in drop_columns were dropped.")
}
  
to_use <- as.data.frame(t(combn(names(df),2)),stringsAsFactors= FALSE)
compare_with<-to_use[[1]]
other <- to_use[[2]]
# Transpose, support pairwise combinations
 
final_res <- Map(function(x,y)
  manymodelr::get_var_corr(df,
                           comparison_var = x,
                           other_vars = y,
                           ...),compare_with,other)



final_result<-do.call(rbind,final_res)
final_result<-structure(final_result,row.names=1:nrow(final_result)) 
if(!is.null(subset_cols)){
  
final_result<-final_result[final_result$comparison_var %in% subset_cols[[1]] &
                           final_result$other_var %in% subset_cols[[2]],]
  
}

final_result
  
}
  

   
 

  


