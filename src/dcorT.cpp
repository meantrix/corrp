#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate Astar
NumericMatrix Astar(const NumericMatrix& d) {
  int n = d.nrow();
  NumericVector m = rowMeans(d);
  double M = mean(d);
  
  // Create a copy of d
  NumericMatrix A = clone(d);
  
  // Subtract row means
  for (int i = 0; i < n; ++i) {
    A(i, _) = A(i, _) - m[i];
  }
  
  // Subtract column means
  for (int j = 0; j < n; ++j) {
    A(_, j) = A(_, j) - m[j];
  }
  
  // Add global mean
  A = A + M;
  
  // Correct diagonal elements
  for (int i = 0; i < n; ++i) {
    A(i, i) = A(i, i) - m[i] + M;
  }
  
  return (n / (n - 1.0)) * A;
}

// Function to calculate bias corrected distance correlation
List BCDCOR(const NumericMatrix& x, const NumericMatrix& y) {
  int n = x.nrow();

  // Compute pairwise distances for x and y
  NumericMatrix xDist = distCpp(x);
  NumericMatrix yDist = distCpp(y);
  
  // Compute Astar for x and y
  NumericMatrix AA = Astar(xDist);
  NumericMatrix BB = Astar(yDist);
  
  // Extract diagonal elements as vectors
  NumericVector diag_AA = diag(AA);
  NumericVector diag_BB = diag(BB);
  
  // Compute BCDCOR components
  double XY = sum(AA * BB) - (n / (n - 2)) * sum(diag_AA * diag_BB);
  double XX = sum(AA * AA) - (n / (n - 2)) * sum(pow(diag_AA, 2));
  double YY = sum(BB * BB) - (n / (n - 2)) * sum(pow(diag_BB, 2));
  double bcR = XY / sqrt(XX * YY);
  
  // Return results as a list
  return List::create(Named("bcR") = bcR,
                      Named("XY") = XY / (n * n),
                      Named("XX") = XX / (n * n),
                      Named("YY") = YY / (n * n),
                      Named("n") = n);
}

// Function to calculate the t statistic for corrected high-dim dCor
double dcorT(const NumericMatrix& x, const NumericMatrix& y) {
  List r = BCDCOR(x, y);
  double Cn = as<double>(r["bcR"]);
  int n = as<int>(r["n"]);
  double M = n * (n - 3) / 2;
  return sqrt(M - 1) * Cn / sqrt(1 - pow(Cn, 2));
}

// Function to perform the dcor t-test of independence for high dimension
List dcorT_test(const NumericMatrix& x, const NumericMatrix& y) {
  List stats = BCDCOR(x, y);
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


NumericMatrix distCpp(NumericMatrix data) {
  int n = data.nrow();
  NumericMatrix distances(n, n);
  
  for (int i = 0; i < n; ++i) {
    for (int j = i; j < n; ++j) {
      double distance = 0.0;
      for (int k = 0; k < data.ncol(); ++k) {
        distance += pow(data(i, k) - data(j, k), 2);
      }
      distances(i, j) = sqrt(distance);
      distances(j, i) = distances(i, j); // Distance matrix is symmetric
    }
  }
  
  return distances;
}