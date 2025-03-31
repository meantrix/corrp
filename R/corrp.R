#' @title corrp compute correlations types analysis in parallel backend.
#'
#' @description Computes correlation-type analysis on large data frames with mixed column types, including integer, numeric, factor, and character. Character columns are treated as categorical variables.\cr
# This function supports parallel processing, allowing faster computations on large datasets. It ensures that different column types are handled appropriately without requiring manual adjustments.\cr
# The method is designed to work efficiently with mixed data, providing a flexible and fast way to analyze relationships between numerical and categorical variables.
#'
#' @name corrp
#'
#' @section Pair Types:
#'
#' **Numeric pairs (integer/numeric):**
#'
#' - **Pearson Correlation Coefficient:** A widely used measure of the strength and direction of linear relationships. Implemented using \code{\link[stats]{cor}}. For more details, see \url{https://doi.org/10.1098/rspl.1895.0041}. The value lies between -1 and 1.\cr
#' - **Distance Correlation:** Based on the idea of expanding covariance to distances, it measures both linear and nonlinear associations between variables. Implemented using \code{\link[energy]{dcorT.test}}. For more details, see \url{https://doi.org/10.1214/009053607000000505}. The value lies between 0 and 1.\cr
#' - **Maximal Information Coefficient (MIC):** An information-based nonparametric method that can detect both linear and non-linear relationships between variables. Implemented using \code{\link[minerva]{mine}}. For more details, see \url{https://doi.org/10.1126/science.1205438}. The value lies between 0 and 1.\cr
#' - **Predictive Power Score (PPS):** A metric used to assess predictive relations between variables. Implemented using \code{\link[ppsr]{score}}. For more details, see \url{https://zenodo.org/record/4091345}. The value lies between 0 and 1.\cr\cr
#'
#' **Numeric and categorical pairs (integer/numeric - factor/categorical):**
#'
#' - **Square Root of R² Coefficient:** From linear regression of the numeric variable over the categorical variable. Implemented using \code{\link[stats]{lm}}. For more details, see \url{https://doi.org/10.4324/9780203774441}. The value lies between 0 and 1.\cr
#' - **Predictive Power Score (PPS):** A metric used to assess predictive relations between numeric and categorical variables. Implemented using \code{\link[ppsr]{score}}. For more details, see \url{https://zenodo.org/record/4091345}. The value lies between 0 and 1.\cr\cr
#'
#' **Categorical pairs (factor/categorical):**
#'
#' - **Cramér's V:** A measure of association between nominal variables. Computed based on a chi-squared test and implemented using \code{\link[lsr]{cramersV}}. For more details, see \url{https://doi.org/10.1515/9781400883868}. The value lies between 0 and 1.\cr
#' - **Uncertainty Coefficient:** A measure of nominal association between two variables. Implemented using \code{\link[DescTools]{UncertCoef}}. For more details, see \url{https://doi.org/10.1016/j.jbi.2010.02.001}. The value lies between 0 and 1.\cr
#' - **Predictive Power Score (PPS):** A metric used to assess predictive relations between categorical variables. Implemented using \code{\link[ppsr]{score}}. For more details, see \url{https://zenodo.org/record/4091345}. The value lies between 0 and 1.\cr
#'
#' @return
#' A list with two tables: `data` and `index`.
#'
#' - **data**: A table containing all the statistical results. The columns of this table are as follows:
#'
#'     - `infer`: The method or metric used to assess the relationship between the variables (e.g., Maximal Information Coefficient or Predictive Power Score).
#'     - `infer.value`: The value or score obtained from the specified inference method, representing the strength or quality of the relationship between the variables.
#'     - `stat`: The statistical test or measure associated with the inference method (e.g., P-value or F1_weighted).
#'     - `stat.value: The numerical value corresponding to the statistical test or measure, providing additional context about the inference (e.g., significance or performance score).
#'     - `isig`: A logical value indicating whether the statistical result is significant (`TRUE`) or not, based on predefined criteria (e.g., threshold for P-value).
#'     - `msg`: A message or error related to the inference process.
#'     - `varx`: The name of the first variable in the analysis (independent variable or feature).
#'     - `vary`: The name of the second variable in the analysis (dependent/target variable).
#'
#'
#'
#' - **index**: A table that contains the pairs of indices used in each inference of the `data` table.
#'
#'
#' All statistical tests are controlled by the confidence interval of p.value parameter. If the statistical tests do not obtain a significance greater/less than p.value the value of variable `isig` will be `FALSE`.\cr
#' If any errors occur during operations the association measure (`infer.value`) will be `NA`.\cr
#' The result `data` and `index` will have \eqn{N^2} rows, where N is the number of variables of the input data.
#' By default there is no statistical significance test for the PPS algorithm. In this case `isig` is NA, you can enable it by setting `ptest = TRUE` in `pps.args`.\cr
#' All the `*.args` can modify the parameters (`p.value`, `comp`, `alternative`, `num.s`, `rk`, `ptest`) for the respective method on it's prefix.
#'
#' @param df \[\code{data.frame(1)}]\cr input data frame.
#' @param parallel \[\code{logical(1)}]\cr If it's TRUE run the operations in parallel backend.
#' @param n.cores \[\code{numeric(1)}]\cr The number of cores to use for parallel execution.
#' @param p.value \[\code{logical(1)}]\cr
#' P-value probability of obtaining the observed results of a test,
#' assuming that the null hypothesis is correct. By default p.value=0.05 (Cutoff value for p-value.).
#' @param comp \[\code{character(1)}]\cr The parameter \code{p.value} must be greater
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
#' `pps: Predictive Power Score`. Default is `Cramer's V`.
#' @param lm.args \[\code{list(1)}]\cr additional parameters for linear model to be passed to \code{\link[stats]{lm}}.
#' @param pearson.args \[\code{list(1)}]\cr additional parameters for Pearson correlation to be passed to \code{\link[stats]{cor.test}}.
#' @param dcor.args \[\code{list(1)}]\cr additional parameters for the distance correlation to be passed to \code{\link[corrp]{dcorT_test}}.
#' @param mic.args \[\code{list(1)}]\cr additional parameters for the maximal information coefficient to be passed to \code{\link[minerva]{mine}}.
#' @param pps.args \[\code{list(1)}]\cr additional parameters for the predictive power score to be passed to \code{\link[ppsr]{score}}.
#' @param uncoef.args \[\code{list(1)}]\cr additional parameters for the uncertainty coefficient to be passed to \code{\link[DescTools]{UncertCoef}}.
#' @param cramersV.args \[\code{list(1)}]\cr additional parameters for the Cramer's V to be passed to \code{\link[lsr]{cramersV}}.
#'
#' @author Igor D.S. Siciliani, Paulo H. dos Santos
#'
#' @keywords correlation, predictive power score, linear model, distance correlation, mic, pearson, cramér's V
#'
#' @references
#' KS Srikanth, sidekicks, cor2, 2020. URL: \url{https://github.com/talegari/sidekicks/}.
#' Paul van der Laken, ppsr, 2021. URL: \url{https://github.com/paulvanderlaken/ppsr/}.
#'
#' @examples
#' # Usage with default settings
#' iris_c <- corrp(iris)
#' iris_m <- corr_matrix(iris_c, isig = FALSE) # You can then make correlation matrix
#' if (require("corrplot")) {
#'   corrplot(iris_m) # You can visualize the matrix using corrplot
#' }
#'
#' # Using PPS for both numeric-numeric and numeric-categorical pairs
#' iris_c1 <- corrp(iris, cor.nn = "pps", cor.nc = "pps")
#'
#' # Using Distance Correlation for numeric-numeric and Predictive Power Score for numeric-categorical
#' iris_c2 <- corrp(iris, cor.nn = "dcor", cor.nc = "pps", dcor.args = list(method = "auto"))
#'
#' # Using Maximal Information Coefficient (MIC) for numeric-numeric and Uncertainty Coefficient for categorical-categorical
#' iris_c3 <- corrp(iris, cor.nn = "mic", cor.cc = "uncoef", mic.args = list(alpha = 0.6))
#'
#' # Using PPS for all pair types
#' iris_c4 <- corrp(iris, cor.nn = "pps", cor.nc = "pps", cor.cc = "pps")
#'
#' # Using Distance Correlation for numeric-numeric, Predictive Power Score for numeric-categorical,
#' # and Uncertainty Coefficient for categorical-categorical
#' iris_c5 <- corrp(
#'   iris,
#'   cor.nn = "dcor", cor.nc = "pps", cor.cc = "uncoef",
#'   dcor.args = list(method = "auto")
#' )
#' @export
corrp <- function(df,
                  parallel = TRUE,
                  n.cores = 1,
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

  # Parallel corr_fun
  if (parallel) {

    is_loaded = "corrp" %in% loadedNamespaces()
    package_path = system.file(package = "corrp") %>% stringr::str_remove("/inst$")
    
    cluster <- parallel::makeCluster(n.cores)
    parallel::clusterExport(cl = cluster, c("is_loaded", "package_path"), envir = environment())
    
    parallel::clusterEvalQ(cluster, {
      if (is_loaded) {
        devtools::load_all(package_path)
      } else {
        library("corrp")
      }
    })

    corr <- parallel::clusterApply(
      cluster, seq_len(NROW(index.grid)),
      function(k) {
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
    # Sequential corr_fun
    corr <- lapply(
      seq_len(NROW(index.grid)),
      function(k) {
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
