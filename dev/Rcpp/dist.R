
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
