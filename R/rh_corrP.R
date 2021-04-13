###############################################################################
#
# rh_corrP
# -- #remove highly correlated variables from a data.frame using the matrix
#corrP correlation matrix.
#
###############################################################################
#
# author  : IDS siciliani (igor-siciliani)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
###############################################################################
#' @title rh_corrP
#'
#' @description remove highly correlated variables from a data.frame using the
#' corrp correlation matrix and the caret package function `caret::findCorrelation` .
#'
#'
#' @param df input data frame
#' @param corrmat correlation matrix from corrP function
#' @param cutoff A numeric value for the pair-wise absolute correlation cutoff.
#' The default values is 0.75.
#'
#' @author IDS siciliani (igor-siciliani)
#'
#' @keywords GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#'
#' @examples
#' \dontrun{
#' air_cor = corrP(airquality)
#' airqualityH = rh_corrP(df=airquality,corrmat=air_cor,cutoff=0.75)
#'}
#' @export
rh_corrP =function(df,corrmat,cutoff=0.75){
  stopifnot(inherits(corrmat,c("corrP","matrix")))
  stopifnot(inherits(df,"data.frame"))

  #the index of the  highly correlated columns (>cutoff)
  rh.idx = caret::findCorrelation(corrmat, cutoff = cutoff)

  #the name of the highly correlated columns (>cutoff)
  cols.rem = colnames(corrmat)[rh.idx]

  #subset original df to remove highly correlated columns (>cutoff)
  df=df[!names(df) %in% cols.rem]

  return(df)
}
