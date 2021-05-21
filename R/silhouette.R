#' @title Silhouette (clustering)
#'
#' @description A C++ implementation of the Silhouette method
#' of interpretation and validation of consistency within acca clusters of data.
#'
#' @param m  [\code{matrix(1)}]\cr correlation matrix from \code{\link{corr_matrix}}
#' @param l [\code{acca_list(1)}]\cr Acca clustering results from \code{\link{acca}}
#'
#' @return [\code{numeric(1)}]\cr the silhouette coefficient for the maximum value of
#'  the mean {\displaystyle s(i)}s(i) over all data of the entire dataset
#'
#'
#' @author Igor D.S. Siciliani
#'
#' @keywords silhouette , acca
#'
#' @references
#' Leonard Kaufman; Peter J. Rousseeuw (1990).
#' Finding groups in data : An introduction to cluster analysis.
#' Hoboken, NJ: Wiley-Interscience. p. 87. doi:10.1002/9780470316801. ISBN 9780471878766.
#'
#' Starczewski, Artur, and Adam Krzyżak. "Performance evaluation of the silhouette index.
#' " International Conference on Artificial Intelligence and Soft Computing. Springer, Cham, 2015.
#'
#'
#' @examples
#' \dontrun{
#'
#' x = corrp::corrp(iris)
#' m = corrp::corr_matrix(x)
#' acca = corrp::acca(m,2)
#' sil_acca(acca,m)
#'
#'}
#'
#' @export
#'
sil_acca = function(acca,...) {
  UseMethod('sil_acca',acca)
}


#' @export
#' @rdname sil_acca
sil_acca.acca_list <- function(acca, m) {

  checkmate::assert_matrix(m)

  if(!inherits(m,"cmatrix")){
    warning("m is not an object of the 'cmatrix' class some results may go wrong.")
  }

  rval = .Call(`_corrp_silhouette_main`, acca, m)

  class(rval) <- c( "corrpstat" )
  attr(rval, "statistic") <- "Silhouette"
  return(rval)

}


#' @export
#' @rdname sil_acca
sil_acca.list <- function(acca, m) {

  checkmate::assert_matrix(m)

  warning("acca is not an object of the 'acca_list' class some results may go wrong. \n")
  if(!inherits(m,"cmatrix")){
    warning("m is not an object of the 'cmatrix' class some results may go wrong.")
  }

  rval = .Call(`_corrp_silhouette_main`, acca, m)

  class(rval) <- c( "corrpstat" )
  attr(rval, "statistic") <- "Silhouette"
  return(rval)

}



