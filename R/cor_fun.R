#' @title Compute Correlation type analysis with Statistical Significance.
#'
#' @description Compute one correlation type analysis on two mixed classes columns of a given dataframe.
#'   The dataframe is allowed to have columns of these four classes: integer,
#'   numeric, factor and character. The character column is considered as
#'   categorical variable. This method is original based on Srikanth KS (talegari) cor2 function.
#'
#' @name cor_fun
#'
#' @aliases cor_fun
#' @param df input data frame.
#' @param p.value \[\code{logical(1)}\]\cr
#' P-value probability of obtaining the observed results of a test,
#' assuming that the null hypothesis is correct. By default p.value=0.05 (Cutoff value for p-value.).
#' @param comp \[\code{character(1)}\]\cr The param \code{p.value} must be greater
#'  or less than those estimated in tests and correlations.
#' @param alternative \[\code{character(1)}\]\cr a character string specifying the alternative hypothesis for
#' the correlation inference. It must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param verbose \[\code{logical(1)}\]\cr Activate verbose mode.
#' @param ... Additional arguments (TODO).
#'
#' @author IDS siciliani (igor-siciliani)
#'
#' @keywords GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#'
#' @references
#' KS Srikanth,sidekicks,cor2, 2020.
#' URL \url{https://github.com/talegari/sidekicks/}.
#'
#' @examples
#' \dontrun{
#' air_cor = corrp(airquality)
#' corrplot::corrplot(air_cor)
#' corrgram::corrgram(air_cor)
#'}
#'
#' @export
cor_fun =  function(df,
                    nx,
                    ny,
                    p.value,
                    verbose = TRUE,
                    ptest.n.sum = 1000,
                    ptest.r = F,
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

  if( cond.cn  && cor.nc == 'lm' ){

    z = x
    clz = clx

    x = y
    clx = cly
    y = z
    cly = clz

    # warning("For the Linear Regression Model the independent variable needs to be the numeric one.","\n",
    #        "In this ",ny," inference by " ,nx, " the order was change.")

    cond.nc = cond.cn

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
            "corpps" = {computeCorN = .corpps

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
            "pps" = { computeCorN = corpps

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
            "corpps" = { computeCorN = .corpps

            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )


  }

  if((class(r) %in% "try-error")){

    if(verbose){
      warnings(cat("ERROR: some operations produces Nas values.","\n",
                   ny, " FUN " ,nx,"\n"))
    }

    r =  list( infer= NA , infer.value = NA , stat = NA, stat.value = NA ,
               isig = 'No', msg = r , var1 = ny, var2 = nx )
  }




  return(r)

}

