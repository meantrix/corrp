
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
A = A - d / n
diag(A) = m - M
(n / (n - 1)) * A

debug(energy::dcorT.test)
Rcpp::sourceCpp("src/dcorT.cpp")
res_cpp = dcorTcpp(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))

res_energy = energy::dcorT.test(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))
class(res_energy) = "list"





Astar <- function(d) {
  ## d is a distance matrix or distance object
  ## modified or corrected doubly centered distance matrices
  ## denoted A* (or B*) in JMVA t-test paper (2013)
  if (inherits(d, "dist"))
    d <- as.matrix(d)
  n <- nrow(d)
  if (n != ncol(d)) stop("Argument d should be distance")
  m <- rowMeans(d)
  M <- mean(d)
  a <- sweep(d, 1, m)
  b <- sweep(a, 2, m)
  A <- b + M  #same as plain A
  #correction to get A^*
  A <- A - d/n
  diag(A) <- m - M
  (n / (n-1)) * A
}
