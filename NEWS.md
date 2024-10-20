# CHANGELOG

## 0.5.0

- Creates the package website with the command: `usethis::use_pkgdown_github_page`;
- Improves test coverage to: 84.34%;
- Clarifies package's authors.

## 0.4.0

### New methods

- `dcorT_test`: Create Correlation Matrix from corrp inferences (C++ wraper).
- `C++ methods`:
  - `Astar`: Derivate the modified distance covariance statistics.
  - `dist`: Calculate distance matrix.
  - `bcdcor`: Function to calculate bias corrected distance correlation.
  - `dcort`: Function to calculate the t statistic for corrected high dimension distance correlation.
  - `dcort_test`: Function to calculate the t statistic for corrected high dimension distance correlation. 
  Returns a list:
    -  method: description of test.
    -  statistic: observed value of the test statistic.
    -  parameter: degrees of freedom.
    -  estimate: (bias corrected) squared distance correlation.
    -  p.value: p-value of the t-test.
    -  data.name: description of data.

### Changes
-  `corr_fun`: Now uses C++ while using distance correlation.

## 0.3.0

- Added C++ implementations of Average correlation clustering algorithm and the Average Silhouette width;
- `acca` New function to clustering correlations;
- `sil_acca` Computes the Average Silhouette width to ACCA clusters;
- `best_acca` Find the optimal number of ACCA clusters;
- Checks ok.

## 0.2.0

- Changed package name `corrP` to `corrp` ;
- Changelog file created ;
- License file GLP3 created;
- Added new correlations types analysis: pps ; dcor ; mic ; uncoef;
- `corrp` function output has a new class `clist` with index matrix and data values;
- `corr_fun`: New function to calculate correlation type inferences to pair of variables;
- `corr_matrix`: New function to create correlation matrix ;
- `corr_rm`: New function to remove highly correlated variables from a data.frame;
- Added verbose param to `corrp` and `corr_fun` functions ; 
- Added testthat unit tests;
- Checks ok;
- Fixed some bugs in function'sand documentations.

