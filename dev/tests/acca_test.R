# ACCA testes C++ functions
# Need to install tictoc package
# install.packages("tictoc")
library(corrp)
tictoc::tic()
x <- corrp::corrp(mtcars)
m <- corrp::corr_matrix(x)
acca_mt <- acca(m, 3)
sil_acca(acca_mt, m)
tictoc::toc()

tictoc::tic()
m <- corrp::corr_matrix(x)
tictoc::toc()

tictoc::tic()
c <- corrp(iris)
m <- corr_matrix(c)
acca_iris <- acca(m, 2)
sil_acca(acca_iris, m)
tictoc::toc()

tictoc::tic()
m <- corrp::corr_matrix(x)
tictoc::toc()
