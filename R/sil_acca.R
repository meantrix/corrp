#' @title Silhouette (Clustering)
#'
#' @description A C++ implementation of the Silhouette method
#' for interpreting and validating consistency within acca clusters of data.
#'
#' @param acca \[\code{acca_list(1)}]\cr Acca clustering results from \code{\link{acca}}.
#' @param m  \[\code{cmatrix(1)|matrix(1)}]\cr A correlation matrix from \code{\link{corr_matrix}}.
#' By default, the distance matrix (dist) used in this method is given by `dist = 1 - m`.
#' @param ... Additional arguments.
#'
#' @return \[\code{numeric(1)}]\cr The average value of
#'  the silhouette width across all data in the entire dataset.
#'  Observations with a large average silhouette width (close to 1)
#'  are very well clustered.
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords silhouette, acca
#'
#' @references
#' Leonard Kaufman; Peter J. Rousseeuw (1990).
#' Finding Groups in Data: An Introduction to Cluster Analysis.
#' Hoboken, NJ: Wiley-Interscience. p. 87. doi:10.1002/9780470316801. ISBN 9780471878766.
#'
#' Starczewski, Artur, and Adam Krzyżak. "Performance Evaluation of the Silhouette Index."
#' International Conference on Artificial Intelligence and Soft Computing. Springer, Cham, 2015.
#'
#' @examples
#'
#' x <- corrp::corrp(iris)
#' m <- corrp::corr_matrix(x)
#' acca <- corrp::acca(m, 2)
#' sil_acca(acca, m)
#'
#' @export

sil_acca <- function(acca, m, ...) {
  assert_required_argument(acca, "The 'acca' argument must be a acca_list object, which is the output from acca function, or it must be a list.")
  assert_required_argument(m, "The 'm' argument must be a cmatrix object, which is the output from corr_matrix function or it must be a matrix.")
  UseMethod("sil_acca", acca)
}


#' @export
#' @rdname sil_acca
sil_acca.acca_list <- function(acca, m, ...) {
  checkmate::assert_matrix(m)

  if (!inherits(m, "cmatrix")) {
    warning("m is not an object of the 'cmatrix' class, so some results may be incorrect.")
  }

  rval <- .Call(`_corrp_silhouette_main`, acca, m)

  class(rval) <- c("corrpstat")
  attr(rval, "statistic") <- "Silhouette"
  return(rval)
}


#' @export
#' @rdname sil_acca
sil_acca.list <- function(acca, m, ...) {
  checkmate::assert_matrix(m)

  warning("acca is not an object of the 'acca_list' class, so some results may be incorrect. \n")
  if (!inherits(m, "cmatrix")) {
    warning("m is not an object of the 'cmatrix' class, so some results may be incorrect.")
  }

  rval <- .Call(`_corrp_silhouette_main`, acca, m)

  class(rval) <- c("corrpstat")
  attr(rval, "statistic") <- "Silhouette"
  return(rval)
}
