#' @title Create Correlation Matrix from corrp Inferences
#'
#' @description Using the results obtained from the corrp function,
#' this function creates a correlation matrix.
#'
#' @param c \[\code{clist}\]\cr Output from the \code{\link{corrp}} function.
#' @param col \[\code{character(1)}\]\cr Specifies the column to be used in the correlation matrix.
#' @param isig \[\code{logical(1)}\]\cr Determines whether values that are not statistically significant
#' should be represented by NA or FALSE in the correlation matrix.
#' @param ... Not used. Included for S3 method consistency.
#'
#' @return \[\code{cmatrix}\]\cr
#' A square matrix with class "cmatrix" where each cell \[i,j\] contains the correlation value between variables i and j.
#' The correlation values are taken from the specified column (infer.value, stat.value, or isig) of the input clist.
#' When \code{isig=TRUE}, only statistically significant correlations are included, others are set to NA.
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords correlation matrix, corrp
#'
#' @examples
#'
#' iris_cor <- corrp(iris)
#' iris_m <- corr_matrix(iris_cor, isig = FALSE)
#' if (require("corrplot")) {
#'   corrplot(iris_m) # You can visualize the matrix using corrplot
#' }

#' @export
corr_matrix <- function(c, ...) {
  assert_required_argument(c, "The 'c' argument must be a clist object, which is the output from corrp.")
  UseMethod("corr_matrix", c)
}

#' @export
#' @rdname corr_matrix
corr_matrix.default <- function(c, col = c("infer.value", "stat.value", "isig"), isig = TRUE, ...) {
  warning("The provided object is not of class 'clist'; some results may be incorrect.")

  .corr_matrix(c = c, col = col, isig = isig)
}

#' @export
#' @rdname corr_matrix
corr_matrix.clist <- function(c, col = c("infer.value", "stat.value", "isig"), isig = TRUE, ...) {
  .corr_matrix(c = c, col = col, isig = isig)
}

.corr_matrix <- function(c, col = c("infer.value", "stat.value", "isig"), isig = TRUE) {
  checkmate::assert_names(names(c), identical.to = c("data", "index"))
  checkmate::assert_logical(isig, len = 1)
  stopifnot(all(unique(c$index$i) == unique(c$index$j)))
  col <- match.arg(col)

  df <- cbind(as.data.frame(c$index), c$data[, c("vary", "varx", "isig", "infer.value", "stat.value")])
  mnames <- unique(df[c("i", "vary")])[, 2]
  len <- length(mnames)

  if (isig) {
    df <- subset(df, isig)
  }

  if (col == "isig") {
    m <- matrix(FALSE, ncol = len, nrow = len)
  } else {
    m <- matrix(NA, ncol = len, nrow = len)
  }

  for (k in seq_len(NROW(df))) {
    m[df$i[k], df$j[k]] <- df[k, col]
  }

  rownames(m) <- mnames
  colnames(m) <- mnames

  return(structure(m, class = c("cmatrix", "matrix")))
}
