
source("./dev/Rcpp/memory_time.R")


test_data_a = data.frame(a = runif(2e4)) %>% as.matrix()
test_data_b = data.frame(b = runif(2e4)) %>% as.matrix()


benchmark_cpp = calculate_memory_runtime({
  dcorT_test(test_data_a, test_data_b)
})


benchmark_r = calculate_memory_runtime({
  energy::dcorT.test(test_data_a, test_data_b)
})