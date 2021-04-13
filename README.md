# corrp

<!-- badges: start -->

[![version](https://img.shields.io/badge/version-0.1.1-green.svg)](https://semver.org)
<!-- badges: end -->

Compute correlations type analysis (Pearson correlation, R^2 coefficient of linear regression,
Cramer's V measure of association,Covariance, Distance Correlation,The Maximal Information Coefficient and Predictive Power Score) of large dataframes with mixed types variables(classes: integer, numeric, factor and character) in parallel backend.

# Details

The [corrp package](https://github.com/meantrix/corrP) under development by Meantrix team and original based on Srikanth KS (talegari) [cor2 function](https://github.com/talegari/sidekicks/) can provide to R users a way to calculate correlation matrix among large data.frames, tibbles or data.tables through a parallel backend.

The data.frame is allowed to have columns of these four classes: integer, numeric, factor and character. The character column is considered as categorical variable.

In this new package the correlation is automatically computed according to the variables types: 

- integer/numeric pair: [Pearson correlation test](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) ;
- integer/numeric - factor/categorical pair: correlation coefficient or squared root of [R^2 coefficient of linear regression](https://en.wikipedia.org/wiki/Coefficient_of_determination);
- factor/categorical pair: [cramersV](https://en.wikipedia.org/wiki/Cramér's_V) a measure of [association](https://en.wikipedia.org/wiki/Association_(statistics)) between two [nominal .](https://en.wikipedia.org/wiki/Nominal_data#Nominal_scale)

Also, the statistical significance of all correlation’s values in the matrix are tested.  If the statistical tests do not obtain a significance level lower than p.value param the null hypothesis can’t be rejected and by default, the correlation between the variable pair will be `NA`.

Example:

<code>

```R
library(corrP)
# run correlation in parallel backend
air_cor = corrP(airquality,parallel = TRUE, n.cores = 4, p.value = 0.05)
corrplot::corrplot(air_cor)
corrgram::corrgram(air_cor)
```

</code>



Another package function  **rh_corrp** can remove highly correlated variables from data.frames using the CorrP matrix.

<code>

```R
air_cor = corrP(airquality)
 airqualityH = rh_corrP(df=airquality,corrmat=air_cor,cutoff=0.5)

setdiff(colnames(airquality),(colnames( airqualityH )))
```

</code>

[1] "Ozone" "Temp" 



The [CoorP package](https://github.com/meantrix/corrP) is still very new, but it is already capable of providing some interesting features. In the next versions we will be including some types of plots to be made with  corrp  correlation matrix .







