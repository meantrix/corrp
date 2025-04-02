#' @title Compute Correlation Type Analysis with Statistical Significance
#'
#' @description Performs correlation type analysis
#' on two mixed-class columns of a given dataframe.
#'   The dataframe can contain columns of four types: integer,
#'   numeric, factor, and character. The character column is considered as
#'   a categorical variable.
#'
#' @name corr_fun
#'
#' @inheritSection corrp Pair Types
#'
#'
#' @return \[\code{list}\]\cr
#' A list with all statistical results.\cr
#' All statistical tests are controlled by the confidence interval of p.value parameter. If the statistical tests do not obtain a significance greater/less than p.value the value of variable `isig` will be `FALSE`.\cr
#' If any errors occur during operations the association measure (`infer.value`) will be `NA`.\cr
#' The result `data` and `index` will have \eqn{N^2} rows, where N is the number of variables of the input data.
#' By default there is no statistical significance test for the PPS algorithm. In this case `isig` is NA, you can enable it by setting `ptest = TRUE` in `pps.args`.\cr
#' All the `*.args` can modify the parameters (`p.value`, `comp`, `alternative`, `num.s`, `rk`, `ptest`) for the respective method on it's prefix.
#'
#'
#' @inheritParams corrp
#' @param nx \[\code{character(1)}\]\cr first variable column name: independent/predictor variable.
#' @param ny \[\code{character(1)}\]\cr second variable column name: dependent/target variable.
#'
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords correlation, power predictive score, linear model, distance correlation, mic, point biserial, pearson, cramer'sV
#'
#' @references
#' KS Srikanth, sidekicks, cor2, 2020.
#' URL \url{https://github.com/talegari/sidekicks/}.
#'
#' Paul van der Laken, ppsr, 2021.
#' URL \url{https://github.com/paulvanderlaken/ppsr}.
#'
#' @examples
#'
#' # since both `nx` and `ny` columns are numerical the method type is defined by `cor.nn`
#' corr_fun(iris, nx = "Sepal.Length", ny = "Sepal.Width", cor.nn = "dcor")
#'
#' @export
corr_fun <- function(df,
                     nx,
                     ny,
                     p.value = 0.05,
                     verbose = TRUE,
                     num.s = 250,
                     rk = FALSE,
                     comp = c("greater", "less"),
                     alternative = c("greater", "less", "two.sided"),
                     cor.nn = c("pearson", "mic", "dcor", "pps"),
                     cor.nc = c("lm", "pps"),
                     cor.cc = c("cramersV", "uncoef", "pps"),
                     lm.args = list(),
                     pearson.args = list(),
                     dcor.args = list(),
                     mic.args = list(),
                     pps.args = list(ptest = FALSE),
                     cramersV.args = list(),
                     uncoef.args = list()) {
  assert_required_argument(df, "The 'df' argument must be a data.frame containing the data to analyze.")
  assert_required_argument(nx, "The 'nx' argument must be a character vector specifying a column name from 'df' for the independent variable(s).")
  assert_required_argument(ny, "The 'ny' argument must be a character string specifying a column name from 'df' for the dependent variable.")

  alternative <- match.arg(alternative)
  cor.nn <- match.arg(cor.nn)
  cor.nc <- match.arg(cor.nc)
  cor.cc <- match.arg(cor.cc)
  comp <- match.arg(comp)
  comp <- substr(comp, 1, 1)
  checkmate::assertDataFrame(df)
  checkmate::assert_character(comp, len = 1, pattern = "l|g")
  alternative <- substr(alternative, 1, 1)
  checkmate::assert_character(alternative, len = 1, pattern = "t|l|g")
  checkmate::assert_logical(verbose, len = 1)
  checkmate::assertNumber(p.value, upper = 1, lower = 0)
  checkmate::assert_list(lm.args)
  checkmate::assert_list(pearson.args)
  checkmate::assert_list(dcor.args)
  checkmate::assert_list(mic.args)
  checkmate::assert_list(pps.args)
  checkmate::assert_list(cramersV.args)
  checkmate::assert_list(uncoef.args)
  checkmate::assert_choice(ny, colnames(df))
  checkmate::assert_choice(nx, colnames(df))

  y <- df[ny]
  x <- df[nx]
  cly <- class(y[[1]])
  clx <- class(x[[1]])

  cond.nn <- (cly %in% c("integer", "numeric") && clx %in% c("integer", "numeric"))
  cond.nc <- (cly %in% c("integer", "numeric") && clx %in% c("factor", "character"))
  cond.cn <- (cly %in% c("factor", "character") && clx %in% c("integer", "numeric"))
  cond.cc <- (cly %in% c("factor", "character") && clx %in% c("factor", "character"))

  if (cond.cn) {
    if (cor.nc == "lm") {
      z <- x
      clz <- clx

      x <- y
      clx <- cly
      y <- z
      cly <- clz
    }

    cond.nc <- cond.cn
    # warning("For the Linear Regression Model the independent variable needs to be the numeric one.","\n",
    #        "In this ",ny," inference by " ,nx, " the order was change.")
  }

  # both are numeric/integer
  if (cond.nn) {
    switch(cor.nn,
      "pearson" = {
        computeCorN <- .corperp
        inf.nm <- "Pearson Correlation"
      },
      "mic" = {
        computeCorN <- .micorp
        inf.nm <- "Maximal Information Coefficient"
      },
      "dcor" = {
        computeCorN <- .dcorp
        inf.nm <- "Distance Correlation"
      },
      "pps" = {
        computeCorN <- .corpps
        inf.nm <- "Predictive Power Score"
      }
    )

    r <- try(
      eval(body(computeCorN), list(), enclos = environment())
    )
  }

  # one is numeric and other is a factor/character
  if (cond.nc) {
    switch(cor.nc,
      "lm" = {
        computeCorN <- .corlm
        inf.nm <- "Linear Model"
      },
      "pps" = {
        computeCorN <- .corpps
        inf.nm <- "Predictive Power Score"
      }
    )

    r <- try(
      eval(body(computeCorN), list(), enclos = environment())
    )
  }

  # both are factor/character
  if (cond.cc) {
    switch(cor.cc,
      "cramersV" = {
        computeCorN <- .cramersvp
        inf.nm <- "Cramer's V"
      },
      "uncoef" = {
        computeCorN <- .uncorp
        inf.nm <- "Predictive Power Score"
      },
      "pps" = {
        computeCorN <- .corpps
        inf.nm <- "Uncertainty coefficient"
      }
    )


    r <- try(
      eval(body(computeCorN), list(), enclos = environment())
    )
  }

  if (inherits(r, "try-error")) {
    msg <- ""
    if (verbose) {
      warnings(cat(
        "ERROR: some operations produces Nas values.", "\n",
        ny, " FUN ", nx, "\n"
      ))
      msg <- attr(r, "condition")$message
    }

    r <- list(
      infer = inf.nm, infer.value = NA, stat = NA, stat.value = NA,
      isig = FALSE, msg = msg, varx = nx, vary = ny
    )
  }

  return(r)
}
