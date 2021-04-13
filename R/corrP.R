###############################################################################
#
# corrP
# -- Compute correlations of columns of a dataframe of mixed types in parallel.
#This Package is based on Srikanth KS (talegari) cor2 function.
#
###############################################################################
#
# author  : IDS siciliani (igor-siciliani)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
# Based on Srikanth KS (talegari) cor2 function
###############################################################################
#' @title corrp
#'
#' @description Compute correlations of columns of a large dataframe of mixed types
#' with parallel backend.
#'   The dataframe is allowed to have columns of these four classes: integer,
#'   numeric, factor and character. The character column is considered as
#'   categorical variable. This Package is based on Srikanth KS (talegari) cor2 function.
#'
#' @details The correlation is computed as follows: \itemize{
#'
#'   \item integer/numeric pair: pearson correlation using `cor` function. The
#'   valuelies between -1 and 1.
#'
#'   \item integer/numeric - factor/categorical pair: correlation coefficient or
#'   squared root of R^2 coefficient of linear regression of integer/numeric
#'   variable over factor/categorical variable using `lm` function. The value
#'   lies between 0 and 1.
#'
#'   \item factor/categorical pair: cramersV value is
#'   computed based on chisq test using `lsr::cramersV` function. The value lies
#'   between 0 and 1.
#'
#'    \item All statistical tests are controlled by the confidence internal of
#'    p.value param. If the statistical tests do not obtain a significance lower
#'    than p.value, by default the correlation between variables will be zero.
#'
#'    \item If any errors occur during operations by default the correlation will be zero.
#'    }
#'
#'   For a comprehensive implementation, use `polycor::hetcor`
#'
#' @param df input data frame
#' @param parallel if is TRUE run the operations in parallel backend.
#' @param n.cores The number of cores to use for parallel execution.
#' @param p.value  p-value probability of obtaining the observed results of a test,
#' assuming that the null hypothesis is correct. By default p.value=0.05.
#'
#' @author IDS siciliani (igor-siciliani)
#'
#' @keywords GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#'
#' @examples
#' \dontrun{

#' air_cor = corrp(airquality)
#' corrplot::corrplot(air_cor)
#' corrgram::corrgram(air_cor)
#'}
#' @export
corrp = function(df, parallel = TRUE, n.cores = 1,p.value = 0.05, verbose = c(TRUE,FALSE), ...){

  verbose = match.arg(verbose)

  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  stopifnot(inherits(p.value, "numeric"))


cor_fun = Vectorize(cor_fun, vectorize.args=c("pos_1", "pos_2"))


 # parallel corr matrix
  if(isTRUE(parallel)){

    doParallel::registerDoParallel(min(parallel::detectCores(),n.cores))
    corrmat=cor_par(df,p.value=p.value,verbose = verbose, ...)
    #force stop
    env = foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
    doParallel::stopImplicitCluster()

    } else {
  # sequential corr matrix
  corrmat = outer(1:NCOL(df)
                   , 1:NCOL(df)
                   , function(x, y){cor_fun(df = df, x,y, p.value = p.value, verbose = verbose, ...)})
  }
  rownames(corrmat) = colnames(df)
  colnames(corrmat) = colnames(df)
  #attribute class
  attr(corrmat, 'class') = c('corrp','matrix')
  return(corrmat)
}
