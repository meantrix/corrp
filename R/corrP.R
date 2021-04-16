#' @title corrp compute correlations types analysis in parallel backend.
#'
#' @description Compute correlations type analysis on mixed classes columns of larges dataframes
#' with parallel backend.
#'   The dataframe is allowed to have columns of these four classes: integer,
#'   numeric, factor and character. The character column is considered as
#'   categorical variable. This method is original based on Srikanth KS (talegari) cor2 function.
#'
#' @details The correlation is computed as follows:
#'
#' \itemize{
#'
#'   \item integer/numeric pair: pearson correlation using `cor` function. The
#'   value lies between -1 and 1.
#'
#'   \item integer/numeric - factor/categorical pair: correlation coefficient or
#'   squared root of R^2 coefficient of linear regression of integer/numeric
#'   variable over factor/categorical variable using `lm` function. The value
#'   lies between 0 and 1.
#'
#'   \item factor/categorical pair: cramersV value is
#'   computed based on chisq test and using `lsr::cramersV` function. The value lies
#'   between 0 and 1.
#'
#'    \item All statistical tests are controlled by the confidence internal of
#'    p.value param. If the statistical tests do not obtain a significance lower/upper
#'    than p.value, by default the correlation between variables will be `NA`.
#'
#'    \item If any errors occur during operations by default the correlation will be `NA`.
#'    }
#'
#'   For a comprehensive implementation, use `polycor::hetcor`
#'
#' @param df input data frame.
#' @param parallel \[\code{logical(1)}\]\cr If its TRUE run the operations in parallel backend.
#' @param n.cores \[\code{numeric(1)}\]\cr The number of cores to use for parallel execution.
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
corrp <-
  function(df, ...) {

    UseMethod("corrp",df)

}


#'@rdname corrp
corrp.data.frame = function(df,
                            parallel = TRUE,
                            n.cores = 1,
                            p.value = 0.05,
                            verbose = TRUE,
                            comp = c("greater","less"),
                            alternative = c("two.sided", "less", "greater"),
                            cor.n = c("pearson","MIC","Dcor","pps"),
                            cor.nc = c("lm","pps"),
                            cor.cc = c("cramersV","pps")
                            ...){

  alternative = match.arg(alternative)
  comp = match.arg(comp)
  comp = substr(comp,1,1)
  checkmate::assert_character(comp,len = 1, pattern = "l|g")
  alternative = substr(alternative,1,1)
  checkmate::assert_character(alternative,len = 1, pattern = "t|l|g")
  checkmate::assert_logical(verbose,len = 1)
  checkmate::assert_logical(parallel,len = 1)
  checkmate::assertNumber(p.value,upper = 1, lower = 0)
  checkmate::assertNumber(n.cores)

  stopifnot( sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))


cor_fun = Vectorize( cor_fun, vectorize.args = c("pos_1", "pos_2") )


 # parallel corr matrix
  if( isTRUE(parallel) ){

    doParallel::registerDoParallel( min(parallel::detectCores(),n.cores) )
    corrmat=cor_par( df,p.value=p.value, ... )
    #force stop
    env = foreach:::.foreachGlobals
    rm( list = ls(name = env), pos = env )
    doParallel::stopImplicitCluster()

    } else {
  # sequential corr matrix
  corrmat = outer(   1:NCOL(df)
                   , 1:NCOL(df)
                   , function(x, y){cor_fun( df = df, x, y, p.value = p.value, ... )}
            )
  }
  rownames(corrmat) = colnames(df)
  colnames(corrmat) = colnames(df)
  #attribute class
  attr(corrmat, 'class') = c('corrp','matrix')
  return(corrmat)
}
