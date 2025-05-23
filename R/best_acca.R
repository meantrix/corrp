#' @title Silhouette (clustering)
#'
#' @description Determining the optimal number of
#' cluster in the ACCA clustering using the
#' average silhouette approach.
#'
#' @param m  \[\code{matrix}\]\cr correlation matrix
#' from \code{\link{corr_matrix}}.
#' @param mink \[\code{integer(1)}\]\cr minimum number of clusters considered.
#' @param maxk \[\code{integer(1)}\]\cr maximum number of clusters considered.
#' @param maxrep \[\code{integer(1)}\]\cr
#' Maximum number of consecutive iterations without changes in the cluster structure before stopping the algorithm.
#' If the clusters remain identical for \code{maxrep} iterations in a row, the algorithm considers that it has converged and stops early, without reaching \code{maxiter}.
#' It serves to speed up execution by detecting when the clustering has stabilized.
#' @param maxiter \[\code{integer(1)}\]\cr
#' Maximum number of iterations allowed for the algorithm.
#' Regardless of cluster stability, the algorithm will be forcibly stopped after \code{maxiter} iterations to prevent infinite loops or excessive computation.
#' It acts as a hard upper limit on the runtime.
#' @param ... Not used. Included for S3 method consistency.
#'
#'
#' @return \[\code{list}\]\cr
#' A `list` which is a list that has three elements:
#'
#' - **silhouette.ave**: The silhouettes average per k.
#'
#' - **k**: The sequence of clusters tested.
#'
#' - **best.k**: The optimal number of clusters.
#'
#'
#'
#' @seealso \code{\link{sil_acca}}
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords silhouette, acca, optimal, k
#'
#' @references
#' Leonard Kaufman; Peter J. Rousseeuw (1990).
#' Finding groups in data : An introduction to cluster analysis.
#' Hoboken, NJ: Wiley-Interscience.
#' p. 87. doi:10.1002/9780470316801. ISBN 9780471878766.
#'
#' Starczewski, Artur, and Adam Krzyżak.
#' "Performance evaluation of the silhouette index.
#' "International Conference on Artificial Intelligence
#' and Soft Computing. Springer, Cham, 2015.
#'
#' @examples
#'
#' x <- corrp::corrp(iris)
#' m <- corrp::corr_matrix(x)
#' best_acca(m, 2, 6)
#'
#' @export
#'
best_acca <- function(m, ...) {
  assert_required_argument(m, "The 'm' argument must be a cmatrix object, which is the output from corr_matrix function, or it must be a matrix.")
  UseMethod("best_acca", m)
}


#' @export
#' @rdname best_acca
best_acca.cmatrix <- function(m, mink, maxk, maxrep = 2L, maxiter = 100L, ...) {
  mink <- as.integer(mink)
  maxk <- as.integer(maxk)
  maxrep <- as.integer(maxrep)
  maxiter <- as.integer(maxiter)
  checkmate::assert_int(mink, lower = 2)
  checkmate::assert_true(mink < maxk)
  checkmate::assert_int(maxrep, lower = 2, upper = maxiter)
  checkmate::assert_int(maxiter, lower = maxrep)

  .Call(`_corrp_best_acca_sil`, m, mink, maxk, maxrep, maxiter)
}

#' @export
#' @rdname best_acca
best_acca.matrix <- function(m, mink, maxk, maxrep = 2L, maxiter = 100L, ...) {
  warning(
    "m is not an object of the 'cmatrix' class, so some results may be incorrect."
  )

  mink <- as.integer(mink)
  maxk <- as.integer(maxk)
  maxrep <- as.integer(maxrep)
  maxiter <- as.integer(maxiter)
  checkmate::assert_int(mink, lower = 2)
  checkmate::assert_true(mink < maxk)
  checkmate::assert_int(maxrep, lower = 2, upper = maxiter)
  checkmate::assert_int(maxiter, lower = maxrep)

  .Call(`_corrp_best_acca_sil`, m, mink, maxk, maxrep, maxiter)
}
