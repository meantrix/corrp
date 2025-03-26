#include "dcort.h"
using namespace Rcpp;


// Modified distance covariance statistics
NumericMatrix Astar(NumericMatrix d) {
  int n = d.nrow();
  NumericVector m = Rcpp::rowMeans(d);

  // Calculate overall mean
  double M = mean(m);

  d = (1.0 - 1.0 / n) * d + M;

  for (int i = 0; i < n; ++i) {
    d(i, _) = d(i, _) - m[i];
    d(_, i) = d(_, i) - m[i];
  }

  // Apply correction
  for (int i = 0; i < n; ++i) {
    d(i, i) = m[i] - M;
  }
  
  d = n / (n - 1.0) * d;

  return d;
}

// Distance matrix
NumericMatrix dist(NumericMatrix data) {
  int n = data.nrow();
  NumericMatrix distances(n, n);

  NumericVector row_i(n);
  NumericVector row_j(n);
  
  for (int i = 0; i < n; ++i) {
    row_i = data(i, _);
    
    for (int j = i; j < n; ++j) {
      row_j = data(j, _);
      
      double distance = sqrt(sum(pow(row_i - row_j, 2)));
      distances(i, j) = distance;
      distances(j, i) = distance;
    }
  }
  
  
  return distances;
}

// Function to calculate bias corrected distance correlation
List bcdcor(const NumericMatrix& x, const NumericMatrix& y) {
  int n = x.nrow();

  // Compute pairwise distances for x and y
  NumericMatrix xDist = dist(x);
  NumericMatrix yDist = dist(y);
  
  // Compute Astar for x and y
  NumericMatrix AA = Astar(xDist);
  NumericMatrix BB = Astar(yDist);
  
  // Extract diagonal elements as vectors
  NumericVector diag_AA = diag(AA);
  NumericVector diag_BB = diag(BB);
  
  // Compute bcdcor components
  double XY = sum(AA * BB) - (n / (n - 2)) * sum(diag_AA * diag_BB);
  double XX = sum(AA * AA) - (n / (n - 2)) * sum(pow(diag_AA, 2));
  double YY = sum(BB * BB) - (n / (n - 2)) * sum(pow(diag_BB, 2));
  double bcR = XY / sqrt(XX * YY);
  
  // Return results as a list
  return List::create(Named("bcR") = bcR,                      
                      Named("n") = n);
}

// Function to calculate the t statistic for corrected high-dim dCor
double dcort(const NumericMatrix& x, const NumericMatrix& y) {
  List r = bcdcor(x, y);
  double Cn = as<double>(r["bcR"]);
  int n = as<int>(r["n"]);
  double M = n * (n - 3) / 2;
  return sqrt(M - 1) * Cn / sqrt(1 - pow(Cn, 2));
}

// Function to perform the dcor t-test of independence for high dimension
List dcort_test(const NumericMatrix& x, const NumericMatrix& y) {
  List stats = bcdcor(x, y);
  double bcR = as<double>(stats["bcR"]);
  int n = as<int>(stats["n"]);
  double M = n * (n - 3) / 2;
  int df = M - 1;
  double tstat = sqrt(M - 1) * bcR / sqrt(1 - pow(bcR, 2));  
  double pval = 1 - R::pt(tstat, df, 1, 0);
  return List::create(Named("statistic") = tstat,
                      Named("parameter") = df,
                      Named("p.value") = pval,
                      Named("estimate") = bcR,
                      Named("method") = "dcor t-test of independence for high dimension",
                      Named("data.name") = "x and y");
}


