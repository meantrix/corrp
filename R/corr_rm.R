#' @title Filter Data Frame by Correlation Values
#'
#' @description Remove highly correlated variables from a data frame using the
#' correlation functions' outputs and the \code{\link[caret]{findCorrelation}} function
#' from the caret package in order to reduce pair-wise correlations. This function is
#' suited for mitigating multicollinearity issues in statistical models.
#'
#' @param df \[\code{data.frame(1)}\]\cr The input data frame.
#' @param c \[\code{clist(1)} | \code{cmatrix}\]\cr A correlation list output from the \code{\link[corrp]{corrp}} function (with class \code{clist}),
#' or a correlation matrix output from the \code{\link[corrp]{corr_matrix}} function (with class \code{cmatrix}).
#' @param cutoff \[\code{numeric(1)}\]\cr A numeric value for the pairwise absolute correlation cutoff.
#' The default value is 0.75.
#' @param col \[\code{character(1)}\]\cr The column to be used in the correlation matrix.
#' @param isig \[\code{logical(1)}\]\cr Whether values that are not statistically significant should
#' be represented by \code{NA} or \code{FALSE} in the correlation matrix.
#' @param ... Additional arguments.
#'
#' @return \[\code{data.frame}\]\cr
#' A data frame with highly correlated variables removed based on the specified cutoff.
#'
#' @examples
#'
#' iris_clist <- corrp(iris)
#' iris_cmatrix <- corr_matrix(iris_clist)
#' corr_rm(df = iris, c = iris_clist, cutoff = 0.75, col = "infer.value", isig = FALSE)
#' corr_rm(df = iris, c = iris_cmatrix, cutoff = 0.75, col = "infer.value", isig = FALSE)
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords highly correlated, cmatrix, clist
#'
#' @export
corr_rm <- function(df, c, ...) {
  assert_required_argument(df, "The 'df' argument must be a data frame from which columns will be filtered.")
  assert_required_argument(c, "The 'c' argument must be a clist object, which is the output from corrp, or a cmatrix object, which is the output from corr_matrix.")
  UseMethod("corr_rm", c)
}

#' @export
#' @rdname corr_rm
corr_rm.clist <- function(df, c, col = c("infer.value", "stat.value"),
                          isig = TRUE, cutoff = 0.75, ...) {
  col <- match.arg(col)

  m <- corr_matrix(c = c, col = col, isig = isig, ...)
  rm <- .corr_rm(df = df, m = m, cutoff = cutoff)

  return(rm)
}

#' @export
#' @rdname corr_rm
corr_rm.list <- function(df, c, col = c("infer.value", "stat.value"),
                         isig = TRUE, cutoff = 0.75, ...) {
  warning("This is not an object of the 'clist' class; some results may be incorrect.")

  col <- match.arg(col)

  m <- corr_matrix(c = c, col = col, isig = isig, ...)
  rm <- .corr_rm(df = df, m = m, cutoff = cutoff)

  return(rm)
}


#' @export
#' @rdname corr_rm
corr_rm.cmatrix <- function(df, c, cutoff = 0.75, ...) {
  rm <- .corr_rm(df = df, m = c, cutoff = cutoff)

  return(rm)
}

#' @export
#' @rdname corr_rm
corr_rm.matrix <- function(df, c, cutoff = 0.75, ...) {
  warning("This is not an object of the 'cmatrix' class; some results may be incorrect.")

  rm <- .corr_rm(df, c, cutoff)

  return(rm)
}

.corr_rm <- function(df, m, cutoff = 0.75) {
  checkmate::assertDataFrame(df)

  # The index of the highly correlated columns (>cutoff)
  rh.idx <- caret::findCorrelation(m, cutoff = cutoff)

  # The names of the highly correlated columns (>cutoff)
  cols.rem <- colnames(m)[rh.idx]

  # Subset the original df to remove the highly correlated columns (>cutoff)
  df <- df[!names(df) %in% cols.rem]

  return(df)
}
