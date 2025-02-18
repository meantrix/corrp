#' @useDynLib corrp, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppArmadillo armadillo_version
#' @importFrom corrplot corrplot


#' @title Average correlation clustering algorithm
#'
#' @description A C++ implementation of the Average Correlation Clustering Algorithm (ACCA) \url{https://www.sciencedirect.com/science/article/pii/S1532046410000158}, originally developed for genetic studies using Pearson correlation as a similarity measure. Unlike traditional clustering methods that rely on distance metrics such as Euclidean or Mahalanobis distance, ACCA groups data based on correlation patterns.\cr
#' This implementation works directly with the correlation matrix derived from the \code{\link{corr_matrix}} function and supports mixed data types along with various correlation methods.\cr
#' ACCA is an unsupervised clustering method, meaning it identifies patterns without predefined labels. Similar to k-means, it requires defining the K parameter, which controls the number of clusters.
#'
#' @param m  \[\code{matrix(1)}]\cr correlation matrix from
#' \code{\link{corr_matrix}} or a distance matrix.
#' @param k \[\code{integer(1)}]\cr number of clusters considered.
#' @param maxrep \[\code{integer(1)}]\cr maximum number of
#' interactions without change in the clusters.
#' @param maxiter \[\code{integer(1)}]\cr maximum number of interactions.
#' @param ... Not used. Included for S3 method consistency.
#'
#' @return \[\code{acca_list(k)}]\cr A list with the
#' final result of the clustering method.
#'  That is, every element of the list group names of the variables belonging to each cluster k.
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords correlation, acca
#'
#' @references
#' Bhattacharya, Anindya, and Rajat K. De.
#' "Average correlation clustering algorithm (ACCA) for grouping of co-regulated
#' genes with similar pattern of variation in their expression values."
#' Journal of Biomedical Informatics 43.4 (2010): 560-568.
#'
#' @examples
#'
#' # Clustering a correlation matrix with 3 clusters
#' x <- corrp::corrp(iris)
#' m <- corrp::corr_matrix(x)
#' result <- corrp::acca(m, k = 3)
#' print(result)
#'
#' # Clustering with 5 clusters and increasing the maximum number of interactions
#' x <- corrp::corrp(iris)
#' m <- corrp::corr_matrix(x)
#' result <- corrp::acca(m, k = 5, maxiter = 200)
#' print(result)
#'
#'
#' # Adjusting the maximum number of iterations without change in clusters
#' x <- corrp::corrp(iris)
#' m <- corrp::corr_matrix(x)
#' result <- corrp::acca(m, k = 2, maxrep = 50)
#' print(result)
#'
#'
#' @export
#'
acca <- function(m, k, ...) {
  assert_required_argument(m, "The 'm' argument must be a cmatrix object, which is the output from corr_matrix function, or it must be a matrix.")
  assert_required_argument(m, "The 'k' argument must be the number of clusters considered.")
  UseMethod("acca", m)
}


#' @export
#' @rdname acca
acca.cmatrix <- function(m, k, maxrep = 2L, maxiter = 100L, ...) {
  k <- as.integer(k)
  maxrep <- as.integer(maxrep)
  maxiter <- as.integer(maxiter)
  checkmate::assert_int(k, lower = 2)
  checkmate::assert_int(maxrep, lower = 2, upper = maxiter)
  checkmate::assert_int(maxiter, lower = maxrep)

  allint <- .Call(`_corrp_acca_main`, m, k, maxrep, maxiter)
  res <- allint[length(allint)][[1]]
  names(res) <- paste0("cluster", 1:k)
  return(structure(res, class = c("acca_list", "list")))
}

#' @export
#' @rdname acca
acca.matrix <- function(m, k, maxrep = 2L, maxiter = 100L, ...) {
  warning(
    "m is not an object of the 'cmatrix' class, so some results may be incorrect."
  )

  k <- as.integer(k)
  maxrep <- as.integer(maxrep)
  maxiter <- as.integer(maxiter)
  checkmate::assert_int(k, lower = 2)
  checkmate::assert_int(maxrep, lower = 2, upper = maxiter)
  checkmate::assert_int(maxiter, lower = maxrep)

  allint <- .Call(`_corrp_acca_main`, m, k, maxrep, maxiter)
  res <- allint[length(allint)][[1]]
  names(res) <- paste0("cluster", 1:k)
  return(structure(res, class = c("acca_list", "list")))
}
