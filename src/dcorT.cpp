#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate Astar
NumericMatrix Astar(NumericMatrix d) {
  int n = d.nrow();
  NumericVector m(n);
  double M = 0.0;

  // Calculate row means
  for (int i = 0; i < n; ++i) {
    double row_sum = 0.0;
    for (int j = 0; j < n; ++j) {
      row_sum += d(i, j);
    }
    m[i] = row_sum / n;
  }

  // Calculate overall mean
  for (int i = 0; i < n; ++i) {
    M += m[i];
  }
  M /= n;

  // Calculate Astar matrix
  NumericMatrix A(n, n);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      A(i, j) = d(i, j) - m[i] - m[j] + M - d(i,j) / n;
    }
  }

  // Apply correction
  for (int i = 0; i < n; ++i) {
    A(i, i) = m[i] - M;
  }
  
  A = n / (n - 1.0) * A;

  return A;
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

// Function to calculate bias corrected distance correlation
List BCDCOR(const NumericMatrix& x, const NumericMatrix& y) {
  int n = x.nrow();

  // Compute pairwise distances for x and y
  NumericMatrix xDist = distCpp(x);
  NumericMatrix yDist = distCpp(y);

  for (int i = 0; i < xDist.nrow(); i++)
  {
    for (int j = 0; j < xDist.ncol(); j++)
    {
        std::cout << xDist(i,j) << " ";
    }
      
    // Newline for new row
    std::cout << std::endl;
  }
  
  // Compute Astar for x and y
  NumericMatrix AA = Astar(xDist);
  NumericMatrix BB = Astar(yDist);
  
  // Extract diagonal elements as vectors
  NumericVector diag_AA = diag(AA);
  NumericVector diag_BB = diag(BB);

  Rcpp::Rcout << "The value of diag_AA : " << diag_AA << "\n";
  Rcpp::Rcout << "The value of diag_BB : " << diag_BB << "\n";

  Rcpp::Rcout << "The value of AA : " << "\n";

  for (int i = 0; i < AA.nrow(); i++)
  {
    for (int j = 0; j < AA.ncol(); j++)
    {
        std::cout << AA(i,j) << " ";
    }      
    // Newline for new row
    std::cout << std::endl;
  }

  
  
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

// [[Rcpp::export]]
List dcorTcpp(const NumericMatrix& x, const NumericMatrix& y) { 
  return dcorT_test(x, y );
}


// [[Rcpp::export]]
NumericMatrix Astarcpp(NumericMatrix& d)  { 
  return Astar(d);
}