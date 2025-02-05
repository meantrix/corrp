
library(corrp)
source("./dev/memory_time.R")

test_data_a = data.frame(a = runif(1e4)) 
test_data_b = data.frame(b = runif(1e4))

benchmark_cpp = calculate_memory_runtime({
  dcorT_test(test_data_a, test_data_b)
})
# MEMORY PEAK(mb): 4701.44140625
# TIME (S): 6.022

benchmark_r = calculate_memory_runtime({
  energy::dcorT.test(test_data_a, test_data_b)
})
# MEMORY PEAK(mb): 7000.65234375
# TIME (S): 13.846

test_data_a = data.frame(a = runif(2e4))
test_data_b = data.frame(b = runif(2e4))

benchmark_cpp = calculate_memory_runtime({
  dcorT_test(test_data_a, test_data_b)
})
# MEMORY PEAK(mb): 18440.53515625
# TIME (S): 25.977

benchmark_r = calculate_memory_runtime({
  energy::dcorT.test(test_data_a, test_data_b)
})
# MEMORY PEAK(mb): 27598.3828125
# TIME (S): 60.264