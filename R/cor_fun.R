#' @title Compute Correlation type analysis with Statistical Significance.
#'
#' @description Compute one correlation analysis on two mixed classes columns of a given dataframe.
#'   The dataframe is allowed to have columns of these four classes: integer,
#'   numeric, factor and character. The character column is considered as
#'   categorical variable.
#'
#' @name corr_fun
#'
#' @section Details (Types):
#'
#' - \code{integer/numeric pair} Pearson Correlation using \code{\link[stats]{cor}} function. The
#'   value lies between -1 and 1.\cr
#' - \code{integer/numeric pair} Distance Correlation using \code{\link[energy]{dcorT.test}} function. The
#'   value lies between 0 and 1.\cr
#' - \code{integer/numeric pair} Maximal Information Coefficient using \code{\link[minerva]{mine}} function. The
#'   value lies between 0 and 1.\cr
#' - \code{integer/numeric pair} Predictive Power Score using \code{\link[ppsr]{score}} function. The
#'   value lies between 0 and 1.\cr\cr
#' - \code{integer/numeric - factor/categorical pair} correlation coefficient or
#'   squared root of R^2 coefficient of linear regression of integer/numeric
#'   variable over factor/categorical variable using \code{\link[stats]{lm}} function. The value
#'   lies between 0 and 1.\cr
#' - \code{integer/numeric - factor/categorical pair} Predictive Power Score using \code{\link[ppsr]{score}} function. The
#'   value lies between 0 and 1.\cr\cr
#' - \code{factor/categorical pair} Cramer's V value is
#'   computed based on chisq test and using \code{\link[lsr]{cramersV}} function. The value lies
#'   between 0 and 1.\cr
#' - \code{factor/categorical pair} Uncertainty coefficient using \code{\link[DescTools]{UncertCoef}} function. The
#'   value lies between 0 and 1.\cr
#' - \code{factor/categorical pair} Predictive Power Score using \code{\link[ppsr]{score}} function. The
#'   value lies between 0 and 1.\cr
#'
#' @section Details (Statistics):
#' - All statistical tests are controlled by the confidence internal of
#'   p.value param. If the statistical tests do not obtain a significance grater/less
#'   than p.value, by default the value of variable `isig` will be `FALSE`.\cr
#' - There is no statistical significance test for the pps algorithm. By default isig = TRUE.\cr
#' - If any errors occur during operations by default the correlation will be `NA`.
#'
#' @param df \[\code{data.frame(1)}\]\cr input data frame.
#' @param nx \[\code{character(1)}\]\cr column name of  dependent/predictor variable.
#' @param ny \[\code{character(1)}\]\cr column name of  independent/target variable.
#' @param p.value \[\code{logical(1)}\]\cr
#' P-value probability of obtaining the observed results of a test,
#' assuming that the null hypothesis is correct. By default p.value=0.05 (Cutoff value for p-value.).
#' @param comp \[\code{character(1)}\]\cr The param \code{p.value} must be greater
#'  or less than those estimated in tests and correlations.
#' @param alternative \[\code{character(1)}\]\cr a character string specifying the alternative hypothesis for
#' the correlation inference. It must be one of "two.sided" (default), "greater" or "less".
#' You can specify just the initial letter.
#' @param verbose \[\code{logical(1)}\]\cr Activate verbose mode.
#' @param num.s \[\code{numeric(1)}\]\cr Used in permutation test. The number of samples with
#' replacement created with y numeric vector.
#' @param rk \[\code{logical(1)}\]\cr Used in permutation test.
#' if its TRUE transform x, y numeric vectors with samples ranks.
#' @param cor.nn  \[\code{character(1)}\]\cr
#' Choose correlation type to be used in integer/numeric pair inference.
#' The options are `pearson: Pearson Correlation`,`mic: Maximal Information Coefficient`,
#' `dcor: Distance Correlation`,`pps: Predictive Power Score`.Default is `Pearson Correlation`.
#' @param cor.nc  \[\code{character(1)}\]\cr
#' Choose correlation type to be used in integer/numeric - factor/categorical pair inference.
#' The option are `lm: Linear Model`,`pps: Predictive Power Score`. Default is `Linear Model`.
#' @param cor.cc  \[\code{character(1)}\]\cr
#' Choose correlation type to be used in factor/categorical pair inference.
#' The option are `cramersV: Cramer's V`,`uncoef: Uncertainty coefficient`,
#' `pps: Predictive Power Score`. Default is ` Cramer's V`.
#' @param lm.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param pearson.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param dcor.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param mic.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param pps.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param uncoef.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param cramersV.args \[\code{list(1)}\]\cr additional parameters for the specific method.
#' @param ... Additional arguments (TODO).
#'
#' @author Igor D.S. Siciliani
#'
#' @keywords correlation , power predictive score , linear model , distance correlation ,
#' mic , point biserial , pearson , cramer's V
#'
#' @references
#' KS Srikanth,sidekicks,cor2, 2020.
#' URL \url{https://github.com/talegari/sidekicks/}.
#'
#'
#' Paul van der Laken, ppsr,2021.
#' URL \url{https://github.com/paulvanderlaken/ppsr}.
#'
#' @examples
#' \dontrun{
#'
#' corr_fun(iris,nx = "Sepal.Length",ny = "Sepal.Width",cor.nn = "dcor")
#'
#'}
#'
#' @export
corr_fun =  function(df,
                    nx,
                    ny,
                    p.value = 0.05,
                    verbose = TRUE,
                    num.s = 1000,
                    rk = F,
                    comp = c("greater","less"),
                    alternative = c("two.sided", "less", "greater"),
                    cor.nn = c("pearson","mic","dcor","pps"),
                    cor.nc = c("lm","pps"),
                    cor.cc = c("cramersV","uncoef","pps"),
                    lm.args = list(),
                    pearson.args = list(),
                    dcor.args = list(),
                    mic.args = list(),
                    pps.args = list(),
                    cramersV.args = list(),
                    uncoef.args = list(),
                   ...){


  alternative = match.arg(alternative)
  cor.nn = match.arg(cor.nn)
  cor.nc = match.arg(cor.nc)
  cor.cc = match.arg(cor.cc)
  comp = match.arg(comp)
  comp = substr(comp,1,1)
  checkmate::assertDataFrame(df)
  checkmate::assert_character(comp,len = 1, pattern = "l|g")
  alternative = substr(alternative,1,1)
  checkmate::assert_character(alternative,len = 1, pattern = "t|l|g")
  checkmate::assert_logical(verbose,len = 1)
  checkmate::assertNumber(p.value,upper = 1, lower = 0)
  checkmate::assert_list(lm.args)
  checkmate::assert_list(pearson.args)
  checkmate::assert_list(dcor.args)
  checkmate::assert_list(mic.args)
  checkmate::assert_list(pps.args)
  checkmate::assert_list(cramersV.args)
  checkmate::assert_list(uncoef.args)
  checkmate::assert_choice(ny,colnames(df))
  checkmate::assert_choice(nx,colnames(df))



  y = df[ny]
  x = df[nx]
  cly = class(y[[1]])
  clx = class(x[[1]])

  cond.nn = ( cly %in% c("integer", "numeric") && clx %in% c("integer", "numeric") )
  cond.nc = ( cly %in% c("integer", "numeric") && clx %in% c("factor", "character") )
  cond.cn = ( cly %in% c("factor", "character") && clx %in% c("integer", "numeric") )
  cond.cc = ( cly %in% c("factor", "character") && clx %in% c("factor", "character") )

  if( cond.cn ){

    if(cor.nc == 'lm'){
      z = x
      clz = clx

      x = y
      clx = cly
      y = z
      cly = clz
    }

    cond.nc = cond.cn
    # warning("For the Linear Regression Model the independent variable needs to be the numeric one.","\n",
    #        "In this ",ny," inference by " ,nx, " the order was change.")
  }

  # both are numeric/integer
  if( cond.nn ){

    switch (cor.nn,
            "pearson" = {computeCorN = .corperp
            },
            "mic" = { computeCorN = .micorp

            },
            "dcor" = { computeCorN = .dcorp

            },
            "pps" = {computeCorN = .corpps

            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )

  }

  # one is numeric and other is a factor/character
  if( cond.nc ){

    switch (cor.nc,
            "lm" = {computeCorN = .corlm
            },
            "pps" = { computeCorN = .corpps
            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )


  }

  # both are factor/character
  if( cond.cc ){

    switch (cor.cc,
            "cramersV" = {computeCorN = .cramersvp
            },
            "uncoef" = { computeCorN = .uncorp
            },
            "pps" = { computeCorN = .corpps
            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )


  }

  if((class(r) %in% "try-error")){

    msg = ""

    if(verbose){
      warnings(cat("ERROR: some operations produces Nas values.","\n",
                   ny, " FUN " ,nx,"\n"))
      msg = r[[1]]
    }

    r =  list( infer= NA , infer.value = NA , stat = NA, stat.value = NA ,
               isig = FALSE, msg = msg , varx = nx, vary = ny )
  }




  return(r)

}

