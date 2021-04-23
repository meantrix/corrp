#' @title corr_rm filter data.frame by correlation values.
#'
#' @description Remove highly correlated variables from a data.frame using the
#' corrp functions outputs and the caret package function \code{\link[caret]{findCorrelation}}.
#'
#'
#' @param df \[\code{data.frame(1)}\]\cr input data frame
#' @param corr \[\code{clist(1)}\ | \code{cmatrix(1)}\]\cr correlation list  output from \code{\link{corrp}} or
#' correlation matrix output from \code{\link{corr_matrix}}.
#' @param cutoff \[\code{numeric(1)}\]\cr A numeric value for the pair-wise absolute correlation cutoff.
#' The default values is 0.75.
#' @param col \[\code{character(1)}\]\cr choose the column to be used in the correlation matrix
#' @param isig \[\code{logical(1)}\]\cr values that are not statistically significant will
#'be represented by NA or FALSE in the correlation matrix.#' @param ... others parameters.
#'
#' @author IDS siciliani (igor-siciliani)
#'
#' @keywords highly correlated, cmatrix , clist
#'
#' @examples
#' \dontrun{
#' air_cor = corrp(airquality)
#' airqualityH = corr_rm(df=airquality,corr=air_cor,cutoff=0.75,col = 'infer.value',isig = F)
#'}
#' @export
corr_rm = function(df, corr,...) {
  UseMethod('corr_matrix',corr)
}

#' @export
#' @rdname corr_rm
corr_rm.clist = function(df, corr, col=c('infer.value','stat.value'), isig = TRUE, cutoff = 0.75,...) {

  col = match.arg(col)

  m = corr_matrix(c = corr, col = col , isig = isig , ...)
  rm  = .corr_rm(df,m,cutoff)

  return(rm)

}

#' @export
#' @rdname corr_rm
corr_rm.list = function(df, corr, col=c('infer.value','stat.value'), isig = TRUE, cutoff = 0.75,...) {

  warning("it is not an object of the 'clist' class some results may go wrong.")

  col = match.arg(col)

  m = corr_matrix(c = corr, col = col , isig = isig , ...)
  rm  = .corr_rm(df,m,cutoff)

  return(rm)

}


#' @export
#' @rdname corr_rm
corr_rm.cmatrix = function(df,corr,cutoff=0.75,...) {

  rm  = .corr_rm(df,corr,cutoff)

  return(rm)

}

#' @export
#' @rdname corr_rm
corr_rm.matrix = function(df,corr,cutoff=0.75,...) {

  warning("it is not an object of the 'cmatrix' class some results may go wrong.")

  rm  = .corr_rm(df,corr,cutoff)

  return(rm)

}

.corr_rm = function(df,corr,cutoff=0.75){

  checkmate::assertDataFrame(df)

  #the index of the  highly correlated columns (>cutoff)
  rh.idx = caret::findCorrelation(corr, cutoff = cutoff)

  #the name of the highly correlated columns (>cutoff)
  cols.rem = colnames(corr)[rh.idx]

  #subset original df to remove highly correlated columns (>cutoff)
  df=df[!names(df) %in% cols.rem]

  return(df)
}
