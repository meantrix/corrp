---
title: 'corrp: An R package for multiple correlation-like analysis and clustering in mixed data'
tags:
  - R
  - correlation
  - clustering
  - mixed data
  - ACCA
authors:
  - name: Igor Dornelles Schoeller Siciliani
    orcid: 0000-0002-9854-9602
    affiliation: "1, 2"
  - name: Contributor Name
    orcid: 0000-0002-9854-9602
    affiliation: "1, 2"
affiliations:
 - name: Meantrix, Brazil
   index: 1
 - name: Universidade Federal de Santa Catarina, Brazil
   index: 2
date: 16 August 2024
bibliography: paper.bib
---

# Summary

Correlation-like analysis provides an important statistical measure that can describe the size and direction of an association between variables. However, there are few R packages that can efficiently perform this type of analysis on large datasets with mixed data types. The **corrp** package provides a full suite of solutions for computing various kinds of correlation-like measures such as Pearson correlation [@pearson:1895], Distance Correlation [@szekely:2007], Maximal Information Coefficient (MIC) [@reshef:2011], Predictive Power Score (PPS) [@flach:2012], Cramér's V [@cramer:1946], and the Uncertainty Coefficient [@theil:1970]. These methods support the analysis of data frames with mixed classes (integer, numeric, factor, and character). In addition, it offers a C++ implementation of the Average Correlation Clustering Algorithm (ACCA) [@tibshirani:2010], which is original used in genetic studies. In this package, the ACCA algorithm has been extended to work directly with correlation matrices derived from different association methods, depending on the data types and user preferences. Furthermore, the package is designed for parallel processing in R, making it highly efficient for large datasets.



# Statement of need

The corrp package is an R package that provides a flexible and efficient way of doing correlation-like analysis on mixed-type data frames. Most traditional correlation methods in R are applicable only to specific data types or small datasets.**corrp** extends this capability by handling mixed classes, integrating various association methods, and offering clustering directly from the resulting correlation matrix. The package is particularly useful for researchers and data scientists working with complex datasets who require robust and scalable tools for both association analysis and clustering.


# Implementation

R and C++ were combined in the creation of corrp package to merge flexibility of R with the speed of C++ which is used for key operations. The core functionalities include:

- Automatic selection of correlation-like methods based on variable types (numeric, categorical, etc.).
- Parallel processing support using the `foreach` package, thus improving performance on large datasets.
- ACCA clustering algorithm with a modified approach to support mixed data and various correlation methods.
- Functions for creating correlation matrices, removing variables based on significance.

In this package, the association measures can be computed according to the following options:

- **Numeric pairs (integer/numeric):**
  - Pearson correlation coefficient [@pearson:1895], a widely used measure of the strength and direction of linear relationships.
  - Distance Correlation or distance covariance [@szekely:2007], based on the idea of expanding covariance to distances, can measure both linear and nonlinear associations between variables.
  - Maximal Information Coefficient (MIC) [@reshef:2011],  a information-based nonparametric based method that can detect linear or non-linear relationships between variables.
  - Predictive Power Score (PPS) [@flach:2012], a metric used to assess predictive relations between variables.

- **Numeric and categorical pairs (integer/numeric - factor/categorical):**
  - Square root of the R² coefficient from linear regression [@cohen:1983].
  - Predictive Power Score (PPS) [@flach:2012].

- **Categorical pairs (factor/categorical):**
  - Cramér's V [@cramer:1946], a measure of association between nominal variables.
  - Uncertainty Coefficient [@theil:1970], a measure of nominal association between two variables.
  - Predictive Power Score (PPS) [@flach:2012].



# Usage

The `corrp` package provides seven main functions for correlation calculations, clustering, and basic data manipulation:


- `corrp()`: Performs correlation-like analysis with user-specified methods for numeric, categorical, factor, interger and mixed pairs.
- `corr_matrix()`: Generates a correlation matrix from analysis results.
- `corr_rm()`: Removes variables based on p-value significance.
- `acca()`: Implements the ACCA clustering algorithm with added support for mixed data types.
- `sil_acca()`: A generic C++ implementation of the Silhouette method.
- `best_acca()`: Determining the optimal number of clusters in ACCA clustering using the average silhouette approach.


First, we calculate the correlations for the `iris` dataset using the Maximal Information Coefficient for numeric pairs, the Predictive Power Score algorithm for numeric/categorical pairs, and the Uncertainty Coefficient for categorical pairs.

```r
library("corrp")
results = corrp(iris, cor.nn = 'mic', cor.nc = 'pps', cor.cc = 'uncoef', n.cores = 2, verbose = FALSE)

head(results$data)
#                            infer infer.value        stat stat.value  isig msg         varx         vary
# Maximal Information Coefficient   0.9994870     P-value  0.0000000  TRUE     Sepal.Length Sepal.Length
# Maximal Information Coefficient   0.2770503     P-value  0.0000000  TRUE     Sepal.Length  Sepal.Width
# Maximal Information Coefficient   0.7682996     P-value  0.0000000  TRUE     Sepal.Length Petal.Length
# Maximal Information Coefficient   0.6683281     P-value  0.0000000  TRUE     Sepal.Length  Petal.Width
#          Predictive Power Score   0.5591864 F1_weighted  0.7028029  TRUE     Sepal.Length      Species
# Maximal Information Coefficient   0.2770503     P-value  0.0000000  TRUE      Sepal.Width Sepal.Length
```


Using the previous result, we can create a correlation matrix as follows:

```r
m = corr_matrix(results, col = 'infer.value', isig = TRUE)
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

Finally, we can cluster the dataset variables using the ACCA algorithm and the correlation matrix. For example, consider clustering into 2 groups (k = 2):

```r
acca.res = acca(m, 2)
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


# Acknowledgements

We acknowledge the contributions of the Meantrix team (https://github.com/meantrix) and thank Srikanth Komala Sheshachala (https://github.com/talegari) for the initial inspiration from the cor2 function.

# References


