library(corrp)
library(laeken)
data(eusilc)
source("./dev/memory_time.R")


eusilc <- eusilc[1:1000, c("eqSS", "eqIncome", "db040", "rb090")]
colnames(eusilc) <- c("House_Size", "Income", "State", "Sex")

benchmarks_nn <- list()
for (nn in c("pearson", "mic", "dcor", "pps")) {
  message(nn)

  bench <- calculate_memory_runtime({
    corrp(
      eusilc[, c("House_Size", "Income")],
      parallel = FALSE,
      cor.nn = nn
    )
  })
  bench$res <- NULL
  benchmarks_nn[[nn]] <- bench
}

benchmarks_nc <- list()
for (nc in c("lm", "pps")) {
  message(nc)

  bench <- calculate_memory_runtime({
    corrp(
      eusilc[, c("Income", "State")],
      parallel = FALSE,
      cor.nc = nc
    )
  })
  bench$res <- NULL
  benchmarks_nc[[nc]] <- bench
}


benchmarks_cc <- list()
for (cc in c("cramersV", "uncoef", "pps")) {
  message(cc)

  bench <- calculate_memory_runtime({
    corrp(
      eusilc[, c("State", "Sex")],
      parallel = FALSE,
      cor.cc = cc
    )
  })

  bench$res <- NULL
  benchmarks_cc[[cc]] <- bench
}
