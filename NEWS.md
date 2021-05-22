# CHANGELOG


## 0.3.0

- Added C++ implementations of Average correlation clustering algorithm and the Average Silhouette width ;
- `acca` New function to clustering correlations;
- `sil_acca` Computes the Average Silhouette width to ACCA clusters ;
- `best_acca` Find the optimal number of ACCA clusters ;

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
- Fixed some bugs in function'sand documentations;

