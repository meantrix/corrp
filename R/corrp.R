#' @title corrp compute correlations types analysis in parallel backend.
#'
#' @description Compute correlations type analysis on mixed classes columns of larges dataframes
#' with parallel backend.
#' The dataframe is allowed to have columns of these four classes: integer,
#' numeric, factor and character. The character column is considered as
#' categorical variable.
#'
#' @name corrp
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
#' @return list with two tables: data and index.\cr
#' - The `$data` table contains all the statistical results;\cr
#' - The `$index` table contains the pairs of indices used in each inference of the data table.
#' - All statistical tests are controlled by the confidence internal of
#'   p.value param. If the statistical tests do not obtain a significance greater/less
#'   than p.value the value of variable `isig` will be `FALSE`.\cr
#' - There is no statistical significance test for the pps algorithm. By default `isig` is TRUE.\cr
#' - If any errors occur during operations the association measure(`infer.value`) will be `NA`.\cr
#' - The result `data` and `index` will have \eqn{N^2} rows, where N is the number of variables of the input data.
#'
#' @param df \[\code{data.frame(1)}]\cr input data frame.
#' @param parallel \[\code{logical(1)}]\cr If its TRUE run the operations in parallel backend.
#' @param n.cores \[\code{numeric(1)}]\cr The number of cores to use for parallel execution.
#' @param p.value \[\code{logical(1)}]\cr
#' P-value probability of obtaining the observed results of a test,
#' assuming that the null hypothesis is correct. By default p.value=0.05 (Cutoff value for p-value.).
#' @param comp \[\code{character(1)}]\cr The param \code{p.value} must be greater
#'  or less than those estimated in tests and correlations.
#' @param alternative \[\code{character(1)}]\cr a character string specifying the alternative hypothesis for
#' the correlation inference. It must be one of "two.sided" (default), "greater" or "less".
#' You can specify just the initial letter.
#' @param verbose \[\code{logical(1)}]\cr Activate verbose mode.
#' @param num.s \[\code{numeric(1)}]\cr Used in permutation test. The number of samples with
#' replacement created with y numeric vector.
#' @param rk \[\code{logical(1)}]\cr Used in permutation test.
#' if its TRUE transform x, y numeric vectors with samples ranks.
#' @param cor.nn \[\code{character(1)}]\cr
#' Choose correlation type to be used in integer/numeric pair inference.
#' The options are `pearson: Pearson Correlation`,`mic: Maximal Information Coefficient`,
#' `dcor: Distance Correlation`,`pps: Predictive Power Score`.Default is `Pearson Correlation`.
#' @param cor.nc \[\code{character(1)}]\cr
#' Choose correlation type to be used in integer/numeric - factor/categorical pair inference.
#' The option are `lm: Linear Model`,`pps: Predictive Power Score`. Default is `Linear Model`.
#' @param cor.cc  \[\code{character(1)}]\cr
#' Choose correlation type to be used in factor/categorical pair inference.
#' The option are `cramersV: Cramer's V`,`uncoef: Uncertainty coefficient`,
#' `pps: Predictive Power Score`. Default is ` Cramer's V`.
#' @param lm.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param pearson.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param dcor.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param mic.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param pps.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param uncoef.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param cramersV.args \[\code{list(1)}]\cr additional parameters for the specific method.
#' @param ... Additional arguments.
#'
#' @author Igor D.S. Siciliani
#'
#' @keywords correlation , power predictive score , linear model , distance correlation ,
#' mic , point biserial , pearson , cramer'sV
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
#'  iris_c <- corrp(iris)
#'  iris_m <- corr_matrix(iris_c, isig = FALSE)
#'  corrplot::corrplot(iris_m)
#' 
#'
#' @export
corrp <- function(df,
                  parallel = TRUE,
                  n.cores = 1,
                  p.value = 0.05,
                  verbose = TRUE,
                  num.s = 250,
                  rk = FALSE,
                  comp = c("greater", "less"),
                  alternative = c("two.sided", "less", "greater"),
                  cor.nn = c("pearson", "mic", "dcor", "pps"),
                  cor.nc = c("lm", "pps"),
                  cor.cc = c("cramersV", "uncoef", "pps"),
                  lm.args = list(),
                  pearson.args = list(),
                  dcor.args = list(),
                  mic.args = list(),
                  pps.args = list(),
                  cramersV.args = list(),
                  uncoef.args = list(),
                  ...) {
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
  checkmate::assert_logical(parallel, len = 1)
  checkmate::assertNumber(p.value, upper = 1, lower = 0)
  checkmate::assertNumber(n.cores)
  checkmate::assert_list(lm.args)
  checkmate::assert_list(pearson.args)
  checkmate::assert_list(dcor.args)
  checkmate::assert_list(mic.args)
  checkmate::assert_list(pps.args)
  checkmate::assert_list(cramersV.args)
  checkmate::assert_list(uncoef.args)

  on.exit(if (parallel) parallel::stopCluster(cluster))




  stopifnot(
    all(
      vapply(df, class, character(1)) %in%
        c("integer", "numeric", "factor", "character")
    )
  )


  cnames <- colnames(df)
  index.grid <- expand.grid("i" = seq(1, NCOL(df)), "j" = seq(1, NCOL(df)), stringsAsFactors = FALSE)
  # parallel corr matrix
  if (parallel) {
    cluster <- parallel::makeCluster(n.cores)
    parallel::clusterEvalQ(cluster, {
      library(corrp)
    })
    corr <- parallel::clusterApply(
      cluster, seq_len(NROW(index.grid)),
      function(k, ...) {
        ny <- cnames[index.grid[["i"]][k]]
        nx <- cnames[index.grid[["j"]][k]]
        corr_fun(df,
          ny = ny,
          nx = nx,
          p.value = p.value,
          verbose = verbose,
          alternative = alternative,
          comp = comp,
          cor.nn = cor.nn,
          cor.nc = cor.nc,
          cor.cc = cor.cc,
          num.s = num.s,
          rk = rk,
          lm.args = lm.args,
          pearson.args = pearson.args,
          cramersV.args = cramersV.args,
          dcor.args = dcor.args,
          pps.args = pps.args,
          mic.args = mic.args,
          uncoef.args = uncoef.args
        )
      }
    )
  } else {
    # sequential corr
    corr <- lapply(
      seq_len(NROW(index.grid)),
      function(k, ...) {
        ny <- cnames[index.grid[["i"]][k]]
        nx <- cnames[index.grid[["j"]][k]]
        corr_fun(df,
          ny = ny,
          nx = nx,
          p.value = p.value,
          verbose = verbose,
          alternative = alternative,
          comp = comp,
          cor.nn = cor.nn,
          cor.nc = cor.nc,
          cor.cc = cor.cc,
          num.s = num.s,
          rk = rk,
          lm.args = lm.args,
          pearson.args = pearson.args,
          cramersV.args = cramersV.args,
          dcor.args = dcor.args,
          pps.args = pps.args,
          mic.args = mic.args,
          uncoef.args = uncoef.args
        )
      }
    )
  }

  corr <- lapply(corr, .null.to.na)
  corr <- do.call(rbind.data.frame, corr)
  corrp.list <- list(data = corr, index = index.grid)


  return(structure(corrp.list, class = c("clist", "list")))
}
