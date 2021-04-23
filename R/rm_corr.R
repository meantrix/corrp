#' @title rh_corrP
#'
#' @description remove highly correlated variables from a data.frame using the
#' corrp correlation matrix and the caret package function `caret::findCorrelation` .
#'
#'
#' @param df \[\code{data.frame(1)}\]\cr input data frame
#' @param corr \[\code{clist(1)} | \code{cmatrix(1)}\]\cr correlation list from corrp function or matrix from corr_matrix function.
#' @param cutoff \[\code{numeric(1)}\]\cr A numeric value for the pair-wise absolute correlation cutoff.
#' The default values is 0.75.
#' @param ... others parameters.
#'
#' @author IDS siciliani (igor-siciliani)
#'
#' @keywords highly correlated, cmatrix , clist
#'
#' @examples
#' \dontrun{
#' air_cor = corrp(airquality)
#' airqualityH = rm_corr(df=airquality,corr=air_cor,cutoff=0.75,col = 'infer.value',isig = F)
#'}
#' @export
rm_corr = function(df,corr,...) {
  UseMethod('corr_matrix',corr)
}

#' @export
#' @rdname rm_corr
rm_corr.clist = function(df,corr, col=c('infer.value','stat.value','isig'), isig = TRUE,cutoff = 0.75,...) {

  m = corr_matrix(c = corr, col = col , isig = isig , ...)
  rm  = .rm_corr(df,m,cutoff)

  return(rm)

}

#' @export
#' @rdname rm_corr
rm_corr.list = function(df,corr, col=c('infer.value','stat.value','isig'), isig = TRUE,cutoff = 0.75,...) {

  warning("it is not an object of the 'clist' class some results may go wrong.")

  m = corr_matrix(c = corr, col = col , isig = isig , ...)
  rm  = .rm_corr(df,m,cutoff)

  return(rm)

}


#' @export
#' @rdname rm_corr
rm_corr.cmatrix = function(df,corr,cutoff=0.75,...) {

  rm  = .rm_corr(df,corr,cutoff)

  return(rm)

}

#' @export
#' @rdname rm_corr
rm_corr.matrix = function(df,corr,cutoff=0.75,...) {

  warning("it is not an object of the 'clist' class some results may go wrong.")

  rm  = .rm_corr(df,corr,cutoff)

  return(rm)

}

.rem_corr = function(df,corr,cutoff=0.75){

  checkmate::assertDataFrame(df)

  #the index of the  highly correlated columns (>cutoff)
  rh.idx = caret::findCorrelation(corrmat, cutoff = cutoff)

  #the name of the highly correlated columns (>cutoff)
  cols.rem = colnames(corrmat)[rh.idx]

  #subset original df to remove highly correlated columns (>cutoff)
  df=df[!names(df) %in% cols.rem]

  return(df)
}
