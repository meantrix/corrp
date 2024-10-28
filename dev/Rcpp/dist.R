
# Test the distCpp function
set.seed(42)
gc()
test_data = readRDS("dev/Rcpp/x.rds") 
test_data = as.matrix(data.frame(a = c(3, 2, 4, 4)))

Rcpp::sourceCpp("src/dcort.cpp")
# Compute distances using distCpp
dist_r = as.matrix(dist(test_data)) 
dist
all(dist_r == dist_cpp)

dist

all(Astar(dist_cpp) == Astarcpp(dist_cpp))




res_cpp = dcorTcpp(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))

res_energy = energy::dcorT.test(as.matrix(data.frame(a = c(3, 2, 4, 4))), as.matrix(data.frame(b = c(5, 3, 3, 7))))
class(res_energy) = "list"

dcorT_test(as.matrix(test_data), as.matrix(test_data))

energy::dcorT(as.matrix(test_data), as.matrix(test_data))
