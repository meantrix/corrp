# CHANGELOG


## 0.6.0

Dedicated version for the publication of the corrp package in the JOSS.

- Add `VignetteBuilder: knitr` to DESCRIPTION
- Add usefull error message for required parameters.
- Fix C++ `Astar` method.
- Run benchmarks, expand the paper to include statements on resource-intensive options, and incorporate an enhanced version of `energy::dcorT.test`. Also, change the data used in the paper.
- Update paper:
  - Give a more detail explanation of `ACCA` algorithm.
  - Strenghted statement of need.
  - Make a map of correlation → R method.
  - Provide a brief remark on the symmetry of the correlation matrix.
- Update `README.md` according with changes in the paper and functions.

### Methods Added

-  Added method `set_arguments`: Assigns provided arguments from the `args_list` to the parent environment. If an argument is inside the arguments of the methods that calculate statistics, it assigns it on the parent environment, and removes the argument from the list.
-  Added method `assert_required_argument`: Ensures that a required argument is provided. If the argument is missing, it throws an error with a clear message.

### Methods Altered

- Altered messages and make *.args lists be able to alter arguments (`p.value`, `comp`, "alternative", "num.s", "rk") of methods: `.corlm`, `.cramersvp`, `.dcorp`, `.corperp`, `.micorp`, `.uncorp`, `.corpps`.
- Update the `.corpps` method to support p-value testing (`p-test`), which is disabled by default due to its slow performance. When `p-test` is not performed, the `isig` value is set to `NA`. `p-test` can be run assigning an element `ptest = TRUE` to `pps.args` argument.

### Documentation

- Enhanced the documentation for `corrp` and `corr_fun` by including examples, refining the pair type section with additional details and references, and providing a more comprehensive explanation of the output format and its interpretation.
- Improved the documentation for `corr_rm` by adding examples and providing a clearer explanation of the `c` parameter.
- Improved the documentation for `acca` by adding examples and providing a more detailed explanation in the description.
- Added examples of usage in the documentation for: `acca`, `best_acca`, `corrp`, `corr_rm`, `corr_matrix`, `corr_fun`, `ptest`, `sil_acca`.
- Fix grammar and ensure package style cohesion.


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

### Methods Altered
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

