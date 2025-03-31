## Check consistency with energy

R_Astar = function(d) {
  if (inherits(d, "dist"))
    d <- as.matrix(d)
  n <- nrow(d)
  if (n != ncol(d)) stop("Argument d should be distance")
  m <- rowMeans(d)
  M <- mean(d)
  a <- sweep(d, 1, m)
  b <- sweep(a, 2, m)
  A <- b + M

  A <- A - d / n
  diag(A) <- m - M
  (n / (n - 1)) * A
}


Rcpp::sourceCpp("src/dcort.cpp")


# test_data = readRDS("dev/Rcpp/x.rds")
test_data = as.matrix(data.frame(a = c(1, 2, 4, 4)))


# Compute distances using distCpp
r_dist = as.matrix(stats::dist(test_data))
cpp_dist = dist(test_data)
all(r_dist == cpp_dist) # TRUE

r_astar = R_Astar(r_dist)
cpp_astar = Astar(cpp_dist)

all(r_astar == cpp_astar)

res_cpp = dcor_t_test(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))

res_energy = energy::dcorT.test(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))

