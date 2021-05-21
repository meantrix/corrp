#ACCA testes C++ functions
library(corrp)
x = corrp::corrp(iris)
m = corrp::corr_matrix(x)
acca_res = acca(m,2)

sil_acca(acca = acca_res,m)
