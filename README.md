# corrP 0.1v
Compute correlations of columns of a large dataframe of mixed types with parallel backend. 
The dataframe is allowed to have columns of these four classes: integer, numeric, factor and character. 
The character column is considered as categorical variable. This Package is based on Srikanth KS (talegari) cor2 function.

# Details

The correlation is computed as follows:

* integer/numeric pair: pearson correlation using `cor` function. The valuelies between -1 and 1.

* integer/numeric - factor/categorical pair: correlation coefficient or
squared root of R^2 coefficient of linear regression of integer/numeric
variable over factor/categorical variable using `lm` function. The value
lies between 0 and 1.

* factor/categorical pair: cramersV value is computed based on chisq test using `lsr::cramersV` function. The value lies between 0 and 1.

All statistical tests are controlled by the confidence internal of p.value param.
If the statistical tests do not obtain a significance lower
than p.value, by default the correlation between variables will be zero.
If any errors occur during operations by default the correlation will be zero.
