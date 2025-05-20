#' @title Filter Data Frame by Correlation Values
#'
#' @description
#' Remove highly correlated variables from a data frame to reduce pair-wise redundancy
#' and mitigate multicollinearity issues in predictive models. This preprocessing step
#' is especially useful when the goal is prediction rather than interpretation, because
#' hypotheses about each individual predictor are not the primary concern.
#'
#' For example, in a genomic prediction study the authors removed highly correlated SNPs
#' to avoid redundant information when working with thousands of markers, improving
#' training efficiency and predictive performance (Wimmer et al. 2021).
#'
#' In the paper *"A Proposed Data Analytics Workflow and Example Using the R Caret Package"*, 
#' this filtering step is applied before model training, demonstrating how the core function
#' \code{caret::findCorrelation} can be used to identify and remove highly correlated variable pairs.
#'
#' Note that while high correlation can bias algorithms like clustering algorithms toward
#' redundant variables, it is much less problematic for tree-based learners.
#'
#' @param df \[\code{data.frame(1)}\]\cr
#'   The input data frame whose columns will be evaluated and filtered.
#' @param c \[\code{clist(1)} | \code{cmatrix}\]\cr
#'   A correlation list (class \code{clist}) produced by \code{\link[corrp]{corrp}},
#'   or a correlation matrix (class \code{cmatrix}) from \code{\link[corrp]{corr_matrix}}.
#' @param cutoff \[\code{numeric(1)}\]\cr
#'   Absolute correlation threshold above which one of a pair of variables will be dropped.
#'   Defaults to \code{0.75}.
#' @param col \[\code{character(1)}\]\cr
#'   Name of the column in the correlation output to use (e.g., \code{"infer.value"}).
#' @param isig \[\code{logical(1)}\]\cr
#'   If \code{TRUE}, non-significant correlations are set to \code{NA} in the matrix
#'   (or \code{FALSE} in a \code{clist}); otherwise all values are retained.
#' @param ... Additional arguments passed to the \code{corr_rm} methods.
#'
#' @return \code{data.frame}\cr
#'   A filtered version of \code{df} with highly correlated variables removed.
#'
#' @examples
#' iris_clist <- corrp(iris)
#' iris_cmatrix <- corr_matrix(iris_clist)
#' corr_rm(df = iris, c = iris_clist, cutoff = 0.75, col = "infer.value", isig = FALSE)
#' corr_rm(df = iris, c = iris_cmatrix, cutoff = 0.75, col = "infer.value", isig = FALSE)
#'
#' @references
#' Wimmer, V.; Albrecht, T.; Auinger, H.-J.; Schön, C.-C. (2021).
#' Genomic prediction studies in plants and animals: Removing highly correlated SNPs
#' to reduce redundancy. PLoS Genetics, 17(3), e1009243.
#' URL: \url{https://doi.org/10.3389/fgene.2021.611506}
#'
#' Jones, S.; Ye, Z.; Xie, Z.; Root, C.; Prasutchai, T.; Anderson, J.; Roggenburg, M.; Lanham, M. A. (2018).
#' A Proposed Data Analytics Workflow and Example Using the R Caret Package.
#' Midwest Decision Sciences Institute (MWDSI) Conference.
#' URL: \url{https://www.matthewalanham.com/Students/2018_MWDSI_R%20caret%20paper.pdf}
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#' @keywords highly correlated, cmatrix, clist, multicollinearity, preprocessing
#' @export
corr_rm <- function(df, c, ...) {
  assert_required_argument(df, "The 'df' argument must be a data frame from which columns will be filtered.")
  assert_required_argument(c, "The 'c'  argument must be a clist object (from corrp) or a cmatrix object (from corr_matrix).")
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
