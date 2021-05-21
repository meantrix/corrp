#ACCA testes C++ functions
library(corrp)
x = corrp::corrp(mtcars)
m = corrp::corr_matrix(x)
m[is.na(m)] = 0
acca_res = acca(m,3)

sil_acca(acca = acca_res,m)

best_acca(m,2,10)


corrp::best_acca()
