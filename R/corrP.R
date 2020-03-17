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
#' @title corrP
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
#' air_cor <- corrP(airquality)
#' corrplot::corrplot(air_cor)
#' corrgram::corrgram(air_cor)
#'
#' @export
corrP = function(df,parallel=TRUE,n.cores=1,p.value=0.05){

  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  stopifnot(inherits(p.value, "numeric"))

  ##############################################################################
  #auxiliar functions
  lmP=function(y,x,p.value){

    sum.res<-summary(
          stats::lm(y ~ as.factor(x))
          )
    pv<- stats::pf (sum.res$fstatistic[1],sum.res$fstatistic[2],
                    sum.res$fstatistic[3],lower.tail=F)

    if(pv<p.value) {
      r<-sqrt(sum.res[["r.squared"]])
      cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
                "p-value: ",pv,"\n"))
    } else {
      r<-0
      cat(paste("there is no correlation at the confidence level  p-value<",p.value,"\n",
                "p-value: ",pv,"\n"))
    }

    return(r)

  }

  cramersVP=function(y,x,p.value,simulate.p.value = TRUE){
    pv<-stats::chisq.test(y,x,simulate.p.value=simulate.p.value)$p.value

    if(pv<p.value) {
      r<-lsr::cramersV(y,x, simulate.p.value=simulate.p.value)
      cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
                "p-value: ",pv,"\n"))
    } else {
      r<-0
      cat(paste("there is no correlation at the confidence level  p-value<",p.value,"\n",
                "p-value: ",pv,"\n"))
    }

    return(r)

  }

  corPerP=function(y,x,p.value,use){

    res<- stats::cor.test(y,x,use,method="pearson",alternative = "two.sided")
    pv<-res[["p.value"]]

    if(pv<p.value) {
      r<-res[["estimate"]]
      cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
                "p-value: ",pv,"\n"))
    } else {
      r<-0
      cat(paste("there is no correlation at the confidence level  p-value<",p.value,"\n",
                "p-value: ",pv,"\n"))
    }

    return(r)

  }

  #parallel corr matrix
  cor_par <- function (df,p.value) {
    `%dopar%` <- foreach::`%dopar%`
    `%:%` <- foreach::`%:%`
    dim<-NCOL(df)
    corp <- foreach::foreach(i=1:dim,.export='cor_fun') %:%
      foreach::foreach (j=1:dim) %dopar% {
        corp = cor_fun(df,i,j,p.value=p.value)
      }
    matrix(unlist(corp), ncol=ncol(df))
  }

  ##############################################################################

  cor_fun <- function(df,pos_1, pos_2,p.value){

    # both are numeric

    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- try(corPerP(df[[pos_1]]
                      , df[[pos_2]]
                      , p.value = p.value
                      , use = "pairwise.complete.obs")
           )
    }

    # one is numeric and other is a factor/character

    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- try(
        lmP(df[[pos_1]],df[[pos_2]],p.value = p.value)
      )
    }

    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- try(
        lmP(df[[pos_2]],df[[pos_1]],p.value = p.value)
      )
    }

    # both are factor/character

    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
       r <- try(cramersVP(df[[pos_1]], df[[pos_2]],p.value = p.value,
                          simulate.p.value = TRUE))


    }


    if((class(r) %in% "try-error")){
      warnings(cat("some operations produces Nas values it will be replaced by 0.","\n",
                     pos_1, " FUN " ,pos_2,"\n"))
      r<-0
    }
    return(r)
  }

cor_fun <- Vectorize(cor_fun, vectorize.args=c("pos_1", "pos_2"))


 # parallel corr matrix
  if(isTRUE(parallel)){

    doParallel::registerDoParallel(min(parallel::detectCores(),n.cores))
    corrmat<-cor_par(df,p.value=p.value)
    #force stop
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
    doParallel::stopImplicitCluster()

    } else {
  # sequential corr matrix
  corrmat <- outer(1:NCOL(df)
                   , 1:NCOL(df)
                   , function(x, y){cor_fun(df=df,x,y,p.value=p.value)})
  }
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  #attribute class
  attr(corrmat, 'class') <- c('corrP','matrix')
  return(corrmat)
}
