
# Define the distCpp function in R
distCpp = Rcpp::cppFunction('
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
}')

# Test the distCpp function
set.seed(42)
gc()
test_data = readRDS("dev/Rcpp/x.rds") 
test_data = matrix(rnorm(20), ncol = 2)  # Generate random test data
d = dist_cpp = distCpp(as.matrix(test_data))   # Compute distances using distCpp
d = dist_r = dist(test_data)       # Compute distances using dist function in R

if (inherits(d, "dist"))
    d = as.matrix(d)
n = nrow(d)
if (n != ncol(d)) stop("Argument d should be distance")
m = rowMeans(d)
M = mean(d)
a = sweep(d, 1, m)
b = sweep(a, 2, m)
A = b + M  #same as plain A
#correction to get A^*
A = A - d/n
diag(A) = m - M
(n / (n-1)) * A

# Compare the results
all.equal(as.vector(dist_cpp), as.vector(dist_r))  # Check if the results are equal


pt

debug(energy::dcorT.test)
energy::dcorT.test(data.frame(a= c(3,2,4,4)), data.frame(b= c(5,3,3,7)))

dcorT_test(as.matrix(data.frame(a = c(3,2,4,4))), as.matrix(data.frame(b = c(5,3,3,7))))

df
tstat
df


dcorT_testCpp = Rcpp::cppFunction('
List dcorT_test(NumericMatrix x, NumericMatrix y) {
  // Compute pairwise distances for x and y
  int n = x.nrow();
  NumericMatrix xDist(n, n);
  NumericMatrix yDist(n, n);
  
  for (int i = 0; i < n; ++i) {
    for (int j = i; j < n; ++j) {
      double distance_x = 0.0;
      double distance_y = 0.0;
      for (int k = 0; k < x.ncol(); ++k) {
        distance_x += pow(x(i, k) - x(j, k), 2);
        distance_y += pow(y(i, k) - y(j, k), 2);
      }
      xDist(i, j) = sqrt(distance_x);
      xDist(j, i) = xDist(i, j); // Distance matrix is symmetric
      yDist(i, j) = sqrt(distance_y);
      yDist(j, i) = yDist(i, j); // Distance matrix is symmetric
    }
  }
  
  // Compute means
  double mean_x = mean(xDist);
  double mean_y = mean(yDist);
  
  // Compute sums of squares
  double sum_xx = sum(pow(xDist - mean_x, 2));
  double sum_yy = sum(pow(yDist - mean_y, 2));
  double sum_xy = sum((xDist - mean_x) * (yDist - mean_y));
  
  // Compute distance correlation
  double dcor = sum_xy / sqrt(sum_xx * sum_yy);
  
  // Compute t-statistic
  double M = n * (n - 3) / 2;
  int df = M - 1;
  double tstat = sqrt(M - 1) * dcor / sqrt(1 - pow(dcor, 2));
  
  // Compute p-value
  double pval = 1 - R::pt(tstat, df, 1, 0);
  
  // Return results as a list
  return List::create(Named("statistic") = tstat,
                      Named("parameter") = df,
                      Named("p.value") = pval,
                      Named("estimate") = dcor,
                      Named("method") = "dcor t-test of independence for high dimension",
                      Named("data.name") = "x and y");
}')



testcpp = dcorT_testCpp(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))
test = energy::dcorT.test(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))

str(testcpp) 
str(test) 
