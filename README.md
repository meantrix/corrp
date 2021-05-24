# corrp  
<a href='http://meantrix.com'><img src='man/figures/logo.png' align="right" height="139" /></a>
<!-- badges: start -->

[![version](https://img.shields.io/badge/version-0.3.0-green.svg)](https://semver.org)
[![License: GPL3](https://img.shields.io/badge/License-GPL3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

<!-- badges: end -->

Compute multiple types of correlations analysis (Pearson correlation, R^2  coefficient of linear regression, Cramer's V measure of association, Distance Correlation,The Maximal Information Coefficient, Uncertainty coefficient and Predictive Power Score) in large dataframes with mixed columns classes(integer, numeric, factor and character) in parallel R backend.
This package also has an C++ implementation of the Average correlation clustering algorithm [ACCA](https://www.sciencedirect.com/science/article/pii/S1532046410000158) 
that works directly with the correlation matrix. In this sense, this implementation differs from the original,
it works with mixed data and several correlation types methods.

## Details

The [corrp package](https://github.com/meantrix/corrP) under development by Meantrix team and original based on Srikanth KS (talegari) [cor2 function](https://github.com/talegari/sidekicks/) can provide to R users a way to work with correlation analysis among large data.frames, tibbles or data.tables through a R parallel backend and C++ functions.

The data.frame is allowed to have columns of these four classes: integer, numeric, factor and character. The character column is considered as categorical variable.

In this new package the correlation is automatically computed according to the follow options: 

#### integer/numeric pair:
- [Pearson correlation test](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) ;
- [Distance Correlation](https://en.wikipedia.org/wiki/Distance_correlation);
- [Maximal Information Coefficient](https://en.wikipedia.org/wiki/Maximal_information_coefficient);
- [Predictive Power Score](https://github.com/paulvanderlaken/ppsr).

#### integer/numeric - factor/categorical pair:
- correlation coefficient or squared root of [R^2 coefficient of linear regression](https://en.wikipedia.org/wiki/Coefficient_of_determination);
- [Predictive Power Score](https://github.com/paulvanderlaken/ppsr).

#### factor/categorical pair:
- [cramersV](https://en.wikipedia.org/wiki/Cramér's_V) a measure of [association](https://en.wikipedia.org/wiki/Association_(statistics)) between two [nominal .](https://en.wikipedia.org/wiki/Nominal_data#Nominal_scale);
- [Uncertainty coefficient](https://en.wikipedia.org/wiki/Uncertainty_coefficient).
- [Predictive Power Score](https://github.com/paulvanderlaken/ppsr).


Also, all statistical tests are controlled by the sigficance of
p.value param. If the statistical tests do not obtain a significance greater/less
than p.value, by default the output of variable `isig` will be `FALSE`.
There is no statistical significance test for the `pps` algorithm, `isig = TRUE` in this case.
If any errors occur during operations by default the correlation will be `NA`.


### Installation

Before you begin, ensure you have met the following requirement(s):

- You have `R >= 3.6.2` installed.


Install the development version from GitHub:

```r
library('remotes')
remotes::install_github("meantrix/corrp@main")
```

## Basic Usage

`corrp package` provides seven main functions for correlation calculations, clustering and basic data manipulation: `corrp`,
`corr_fun`, `corr_matrix`, `corr_rm`, `acca` , `sil_acca` and `best_acca`.

`corrp` Next, we calculate the correlations for the data set iris using: Maximal Information Coefficient for numeric pair, the Power Predictive Score algorithm for numeric/categorical pair and Uncertainty coefficient for categorical pair.

```r
results = corrp::corrp(iris, cor.nn = 'mic',cor.nc = 'pps',cor.cc = 'uncoef', n.cores = 2 , verbose = FALSE)

head(results$data)
#                            infer infer.value        stat stat.value isig msg         varx         vary
# Maximal Information Coefficient   0.9994870     P-value  0.0000000 TRUE     Sepal.Length Sepal.Length
# Maximal Information Coefficient   0.2770503     P-value  0.0000000 TRUE     Sepal.Length  Sepal.Width
# Maximal Information Coefficient   0.7682996     P-value  0.0000000 TRUE     Sepal.Length Petal.Length
# Maximal Information Coefficient   0.6683281     P-value  0.0000000 TRUE     Sepal.Length  Petal.Width
#          Predictive Power Score   0.5591864 F1_weighted  0.7028029 TRUE     Sepal.Length      Species
# Maximal Information Coefficient   0.2770503     P-value  0.0000000 TRUE      Sepal.Width Sepal.Length

```

`corr_matrix` Using the previous result we can create a correlation matrix as follows:

```r
m = corr_matrix(results,col = 'infer.value',isig = TRUE)
m
#              Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
# Sepal.Length    0.9994870   0.2770503    0.7682996   0.6683281 0.4075487
# Sepal.Width     0.2770503   0.9967831    0.4391362   0.4354146 0.2012876
# Petal.Length    0.7682996   0.4391362    1.0000000   0.9182958 0.7904907
# Petal.Width     0.6683281   0.4354146    0.9182958   0.9995144 0.7561113
# Species         0.5591864   0.3134401    0.9167580   0.9398532 0.9999758
# attr(,"class")
# [1] "cmatrix" "matrix" 
```
Now, we can clustering the data set variables through ACCA and the correlation matrix.
By way of example, consider 2 clusters `k = 2`:

```r
acca.res = acca(m,2)
acca.res
# $cluster1
# [1] "Species"      "Sepal.Length" "Petal.Width" 
# 
# $cluster2
# [1] "Petal.Length" "Sepal.Width" 
# 
# attr(,"class")
# [1] "acca_list" "list"     
```

Also,we can calculate The average silhouette width to the cluster `acca.res`:

```r
sil_acca(acca.res,m)
# [1] -0.02831006
# attr(,"class")
# [1] "corrpstat"
# attr(,"statistic")
# [1] "Silhouette"
```
Observations with a large average silhouette width (almost 1) are very well clustered.


### Contributing to corrp

To contribute to `corrp`, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin corrp/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).

### Bug Reports

If you have detected a bug (or want to ask for a new feature), please file an issue with a minimal [reproducible example](https://www.tidyverse.org/help/#reprex) on [GitHub](https://github.com/meantrix/corrp/issues).

### License

This project uses the following license: [GLP3 License](<https://github.com/meantrix/corrp/blob/master/LICENSE.md>).









