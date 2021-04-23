#' @title Create Correlation Matrix from corrp inferences
#'
#' @description Through the results obtained with corrp function
#' create a correlation matrix.
#'
#' @param c \[\code{corrp.list(1)}\]\cr output from the \code{\link{corrp}} function.
#' @param col \[\code{character(1)}\]\cr choose the column to be used in the correlation matrix
#' @param isig \[\code{logical(1)}\]\cr Values that are not statistically significant will
#'be represented by NA or FALSE in the correlation matrix.
#' @param ... others parameters.
#'
#' @examples
#' \dontrun{
#'
#' air_cor = corrp(airquality)
#' air_m = corr_matrix(air_cor,isig = F)
#' corrplot::corrplot(air_m)
#'
#'}
#'
#'@export
corr_matrix = function(c,...) {
  UseMethod('corr_matrix',c)
}

#'@export
#'@rdname corr_matrix
corr_matrix.default = function(c,col=c('infer.value','stat.value','isig'), isig = TRUE,...) {

  warning("it is not an object of the 'clist' class some results may go wrong.")

  .corr_matrix(c = c , col = col , isig = isig, ...)

}

#'@export
#'@rdname corr_matrix
corr_matrix.clist = function(c,col=c('infer.value','stat.value','isig'), isig = TRUE,...) {

  .corr_matrix(c = c , col = col , isig = isig, ...)

}




.corr_matrix = function(c,col=c('infer.value','stat.value','isig'), isig = TRUE,...){

  checkmate::assert_names( names(c), identical.to = c('data','index'))
  checkmate::assert_logical(isig,len = 1)
  stopifnot(c$index$i == unique(c$index$j) )
  col = match.arg(col)

  df =  cbind( as.data.frame(c$index) ,c$data[,c('var1','var2','isig','infer.value','stat.value')] )
  mnames =  unique(df[c('i','var1')])[,2]
  len = length(mnames)

  if(isig){
    df = subset(df, isig )
  }

  if(col == 'isig') { m = matrix(FALSE, ncol = len, nrow = len) } else { m = matrix(NA, ncol = len, nrow = len) }

  for(k in 1:NROW(df)){

    m[df$i[k],df$j[k]] = df[k,col]

  }

  rownames(m) <- mnames
  colnames(m) <- mnames




  return(m)

}
