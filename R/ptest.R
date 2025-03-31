#' @title Correlation Permutation Test
#' @description Execute a one-sample permutation test on two numeric vectors.
#' One vector is kept constant while the other is "shuffled" by resampling.
#' This approximates the null hypothesis — that there is no dependency or difference between the variables.
#' @param x \[\code{numeric(1)}]\cr A numeric vector.
#' @param y \[\code{numeric(1)}]\cr A numeric vector.
#' @param FUN \[\code{function(1)}]\cr The function to be applied.
#' @param num.s \[\code{numeric(1)}]\cr The number of samples with replacement created from the y numeric vector.
#' @param rk \[\code{logical(1)}]\cr If TRUE, transform x and y numeric vectors with sample ranks.
#' @param alternative \[\code{character(1)}]\cr A character string specifying the alternative hypothesis.
#' Must be one of "greater" (default), "less", or "two.sided". You can specify just the initial letter.
#' @param ... Additional arguments.
#'
#' @examples
#'
#' x <- iris[[1]]
#' y <- iris[[2]]
#' ptest(x, y, FUN = function(x, y) cor(x, y), alternative = "t")
#'
#' @export
ptest <- function(x, y,
                  FUN,
                  rk = FALSE,
                  alternative = c("greater", "less", "two.sided"),
                  num.s = 250, ...) {
  FUN <- match.fun(FUN)
  # check mandatory args
  fargs <- formals(FUN)
  fargs <- fargs[vapply(fargs, is.symbol, FUN.VALUE = TRUE)]
  fargs <- names(fargs)
  fargs <- setdiff(fargs, "...")
  largs <- length(fargs)
  if (largs > 2) stop("FUN maximum number of mandatory arguments is 2.")

  alternative <- match.arg(alternative)
  alternative <- substr(alternative, 1, 1)
  checkmate::assert_character(alternative, len = 1, pattern = "t|l|g")
  checkmate::assert_logical(rk, len = 1)
  checkmate::assert_number(num.s)
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]
  # if (!rk) stopifnot(is.numeric(x), is.numeric(y))
  stopifnot(length(x) == length(y))


  if (rk) {
    x <- rank(x)
    y <- rank(y)
  }

  obs <- FUN(x, y)
  est <- c()
  for (i in 1:num.s) {
    y_i <- sample(y, length(y), replace = TRUE)
    est <- append(est, FUN(x, y_i))
  }

  switch(alternative,
    "l" = {
      p.value <- mean(est <= obs)
    },
    "g" = {
      p.value <- mean(est >= obs)
    },
    "t" = {
      p.value <- min(mean(est >= obs), mean(est <= obs)) * 2
    }
  )

  return(p.value)
}


#' @title Create Correlation Matrix from corrp inferences
#'
#' @description Distance correlation t-test of multivariate independence for high dimension. C++ version of energy::dcorT.test.
#'
#' @param x \[\code{data.frame(1) | matrix(1)}]\cr Data of the first sample.
#' @param y \[\code{data.frame(1) | matrix(1)}]\cr Data of the second sample.
#'
#' @return returns a list containing
#'   \item{method}{description of test}
#'   \item{statistic}{observed value of the test statistic}
#'   \item{parameter}{degrees of freedom}
#'   \item{estimate}{(bias corrected) squared dCor(x,y)}
#'   \item{p.value}{p-value of the t-test}
#'   \item{data.name}{description of data}
#' @export
dcor_t_test <- function(x, y) {
  if (!inherits(x, "matrix")) {
    x <- as.matrix(x)
  }

  if (!inherits(y, "matrix")) {
    y <- as.matrix(y)
  }

  .Call(`_corrp_dcort_test`, x, y)
}
