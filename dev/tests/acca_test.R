#ACCA testes C++ functions
library(corrp)
x = corrp::corrp(mtcars)
m = corrp::corr_matrix(x)
acca_res = acca(m,3)
sil_acca(acca = acca_res,m)



nm = colnames(m)
spl = crand_acca(m,2)
m1 = subset2d(m,spl[[1]],spl[[1]])
single1 = csingle_acca(m,2,spl)
subset2d(m, colnames(m) , colnames(m)[1] )
a = acca_main(m,2,200,500)
l1 = a[1]
l2 = a[2]
identical(l1,l2)
compare_list_cha(l1,l2)
compare_cha(c('oi'),c('oi','oi'))
compare_list_cha(list('oi'),list('oi','oi'))


c = corrp(iris)
m = corr_matrix(c)
acca.result = acca(m,2)

x <- readRDS('dev/pnadcorrp.rds')

tictoc::tic()
m = corrp::corr_matrix(x)
tictoc::toc()

tictoc::tic()
acca = acca_main(m,4)
tictoc::toc()

silhouette_main(acca,m)

2
library(corrp)
c = corrp(iris)
m = corr_matrix(c)
acca.res = acca(m,2)

stest2(acca.res,m)

