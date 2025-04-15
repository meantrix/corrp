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
  - name: Paulo Henrique dos Santos
    orcid: 0009-0004-5273-4142
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

Correlation-like analysis provides an important statistical measure that describes the size and direction of an association between variables. However, there are few R packages that can efficiently perform this type of analysis on large datasets with mixed data types. The `corrp` package provides a full suite of solutions for computing various correlation-like measures, such as Pearson correlation [@pearson:1895], Distance Correlation [@szekely:2007], Maximal Information Coefficient (MIC) [@reshef:2011], Predictive Power Score (PPS) [@pps:2020], Cramér's V [@cramer:1946], and the Uncertainty Coefficient [@theil:1972]. These methods support the analysis of data frames with mixed classes (integer, numeric, factor, and character).

Additionally, it offers a C++ implementation of the Average Correlation Clustering Algorithm (ACCA) [@bhattacharya:2010], which was originally developed for genetic studies using Pearson correlation as a similarity measure. In general, ACCA is an unsupervised clustering method, as it identifies patterns in the data without requiring predefined labels. Moreover, it requires the K parameter to be defined, similar to k-means. One of its main differences compared to other clustering methods is that it operates based on correlations rather than traditional distance metrics, such as Euclidean or Mahalanobis distance.

In this package, the ACCA algorithm has been extended to work directly with correlation matrices derived from different association methods, depending on the data types and user preferences. Furthermore, the package is designed for parallel processing in R, making it highly efficient for large datasets.

# Statement of need

The `corrp` package is an R package that provides a flexible and efficient way of performing correlation-like analysis on mixed-type data frames. These datasets can contain different variable types, such as continuous (numeric), ordinal (ordered categorical), and nominal (unordered categorical) variables, which frequently arise in practical scenarios.

Moreover, most traditional correlation methods in R — such as `cor()` from the *stats package* [@stats:2024], which computes Pearson, Spearman, or Kendall correlations between vectors, matrices, or data frames; and `rcorr()` from the *Hmisc package* [@Hmisc:2025], which efficiently computes Pearson or Spearman correlation matrices — are primarily designed for specific data types and may not generalize well to mixed data or large-scale applications. In this sense, `corrp` extends these capabilities by handling mixed data types, integrating various association methods, and offering clustering directly from the resulting correlation matrix..

The `corrp` package automatically detects the variable types present in the dataset. However, manual intervention is needed to select the appropriate correlation measure for each detected variable pair (numeric pairs, categorical pairs, and numeric-categorical pairs) from the available options, as explained in more detail below.

The package is particularly useful for researchers and data scientists working with complex datasets who require robust and scalable tools for both association analysis and clustering.

# Implementation

The `corrp` package integrates R and C++ to combine the flexibility of R with the speed of C++, optimizing key operations. Its core functionalities include the selection of correlation-like methods based on pairs of variable types (numeric pairs, numeric and categorical pairs, etc.). Users can create correlation matrices, remove variables based on significance, and cluster the correlation matrix using the ACCA clustering algorithm. This algorithm has been modified to support mixed data types and various correlation methods. Also, the package supports parallel processing through the `foreach` package, significantly improving performance on large datasets.

As mentioned before, one can choose between the following options based on the type of variable pair:

- **Numeric pairs (integer/numeric):**
  - Pearson correlation coefficient [@pearson:1895], a widely used measure of the strength and direction of linear relationships.
  - Distance Correlation or distance covariance [@szekely:2007], based on the idea of expanding covariance to distances, can measure both linear and nonlinear associations between variables.
  - Maximal Information Coefficient (MIC) [@reshef:2011],  an information-based nonparametric based method that can detect linear or non-linear relationships between variables.
  - Predictive Power Score (PPS) [@pps:2020], a metric used to assess predictive relations between variables.

- **Numeric and categorical pairs (integer/numeric - factor/categorical):**
  - Square root of the R² coefficient from linear regression [@cohen:1983].
  - PPS [@pps:2020].

- **Categorical pairs (factor/categorical):**
  - Cramér's V [@cramer:1946], a measure of association between nominal variables.
  - Uncertainty Coefficient [@theil:1972], a measure of nominal association between two variables.
  - PPS [@pps:2020].

In R, various statistical functions are available to measure these correlations. Below follows a list of correlation techniques and their corresponding R functions:  

- **Linear Model** → `stats::lm`  
- **Pearson Correlation** → `stats::cor.test`  
- **Distance Correlation** → `corrp::dcor_t_test`  
- **MIC** → `minerva::mine`  
- **PPS** → `ppsr::score`  
- **Uncertainty Coefficient** → `DescTools::UncertCoef`  
- **Cramer's V** → `lsr::cramersV`

An important point to note is that some methods, such as the square root of R², PPS, and the Uncertainty Coefficient, are asymmetric. In other words, the correlation value between two variables, A and B, may not be the same as the correlation between B and A in the correlation matrix.

# Usage

The `corrp` package provides seven main functions for correlation calculations, clustering, and basic data manipulation:

- **corrp**: Performs correlation-like analysis with user-specified methods for numeric, categorical, factor, integer and mixed pairs.
- **corr_matrix**: Generates a correlation matrix from analysis results.
- **corr_rm**: Removes variables based on p-value significance.
- **acca**: Performs the ACCA clustering algorithm with added support for mixed data types.
- **sil_acca**: A C++ implementation of the Silhouette method for interpreting and validating the consistency of clusters within ACCA clusters of data.
- **best_acca**: Determining the optimal number of clusters in ACCA clustering using the average silhouette approach.

We calculate correlations for the *eusilc* dataset using the MIC for numeric pairs, PPS for numeric/categorical pairs, and Uncertainty Coefficient for categorical pairs. This synthetic dataset represents Austrian EU-SILC data on income, demographics, and household characteristics [@laeken:2024].

```r
set.seed(2024)
library("laeken")
library("corrp")
data(eusilc)

eusilc <- eusilc[, c("eqSS", "eqIncome", "db040", "rb090")]
colnames(eusilc) <- c("House_Size", "Income", "State", "Sex")

results <- corrp(
  eusilc, 
  cor.nn = 'dcor', cor.nc = 'lm', cor.cc = 'pps',
  verbose = FALSE
)

head(results$data)
```

<div style="font-size: 9px;">
| infer               | infer.value | stat    | stat.value | isig | msg | varx       | vary       |
| --------------------|--------------|---------|------------|------|-----|------------|------------|
| Distance Correlation| 1.000        | P-value | 0.000      | TRUE |     | House_Size | House_Size |
| Distance Correlation| 0.008        | P-value | 0.000      | TRUE |     | House_Size | Income     |
| Linear Model        | 0.146        | P-value | 3.57e-64   | TRUE |     | House_Size | State      |
| Linear Model        | 0.071        | P-value | 4.79e-18   | TRUE |     | House_Size | Sex        |
| Distance Correlation| 0.008        | P-value | 0.000      | TRUE |     | Income     | House_Size |
| Distance Correlation| 1.000        | P-value | 0.000      | TRUE |     | Income     | Income     |
</div>


When choosing correlation methods, it's important to think about their performance for different pair types. For **numeric pairs**, **Pearson** (`pearson`) is the quickest and most efficient option, while the **Maximal Information Coefficient** (`mic`) is significantly slower, making it less suitable for large datasets. **Distance correlation** (`dcor`) is a better performer than MIC but still not the fastest choice, while **Predictive Power Score** (`pps`) is efficient but may take longer than Pearson. For **numeric-categorical pairs**, the **linear model** (`lm`) typically outperforms `pps`. In **categorical pairs**, **Cramér's V** (`cramersV`), **Uncertainty Coefficient** (`uncoef`), and `pps` are options, with `uncoef` being the slowest of the three.

As the number of columns in the data increases, the runtime will also increase due to the `N * N` scaling. Therefore, the user should choose the methods wisely to ensure efficient performance.

Using the previous result, we can create a correlation matrix as follows:

```r
m <- corr_matrix(results, col = 'infer.value', isig = TRUE)
m
```

|            | House_Size |  Income  |  State  |   Sex   |
|------------|------------|----------|---------|---------|
| House_Size |   1.000    |   0.008  |  0.146  |  0.071  |
| Income     |   0.008    |   1.000  |  0.070  |  0.071  |
| State      |   0.146    |   0.070  |  1.000  |  0.000  |
| Sex        |   0.071    |   0.071  |  0.000  |  1.000  |


```r
# attr(,"class")
# [1] "cmatrix" "matrix" 

```

Finally, we can cluster the dataset variables using the ACCA algorithm and the correlation matrix. For example, consider clustering into 2 groups (k = 2):

```r
acca.res <- acca(m, 2)
acca.res
# $cluster1
# [1] "Sex"    "Income"
# 
# $cluster2
# [1] "State"      "House_Size"
# 
# attr(,"class")
# [1] "acca_list" "list"  
```

## Performance Improvements

When using the `corrp` function with the `dcor` method for numeric pairs (i.e., `cor.nn = "dcor"`), significant improvements in both memory usage and runtime are observed. This is because the `corrp` package uses a C++ implementation of distance correlation (`dcor_t_test`), which is more efficient than the `energy::dcorT.test` function from the `energy` package.

For example, using two vectors of length 10000 and 20000, the benchmarks show the following improvements:

| Method                | 10,000       |                | 20,000           |                  |
|-----------------------|--------------|----------------|------------------|------------------|
|                       | Memory (MB)  | Time (sec)     | Memory (MB)      | Time (sec)       |
| **dcor_t_test (C++)**  | 4701.44      | 6.022          | 18440.54         | 25.977           |
| **energy::dcorT.test**| 7000.65      | 13.846         | 27598.38         | 60.264           |

This highlights a substantial reduction in both memory usage and execution time, making the `corrp` package more scalable for larger datasets when applying distance correlation methods. The memory reduction is particularly important because calculating distance correlation requires constructing a distance matrix of size $N^2$, where $N$ is the length of the input vector. As $N$ grows, the memory demands can quickly become prohibitive.

# Acknowledgements

We acknowledge the contributions of the Meantrix team (https://github.com/meantrix) and thank Srikanth Komala Sheshachala (https://github.com/talegari) for the initial inspiration from the `cor2` function [@sidekicks:2025].

# References

