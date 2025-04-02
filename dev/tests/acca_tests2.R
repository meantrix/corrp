# ACCA testes C++ functions
library(corrp)
x <- corrp::corrp(mtcars)
m <- corrp::corr_matrix(x)
m[is.na(m)] <- 0
acca_res <- acca(m, 3)

sil_acca(acca = acca_res, m)

best_acca(m, 2, 10)

##
results <- corrp::corrp(iris, cor.nn = "mic", cor.nc = "pps", cor.cc = "uncoef", n.cores = 2, verbose = FALSE)
m <- corr_matrix(results, col = "infer.value", isig = TRUE)
acca.res <- acca(m, 2)
acca.res


sil_acca(acca.res, m)
