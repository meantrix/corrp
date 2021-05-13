#ACCA testes C++ functions
library(corrp)
x = corrp::corrp(iris)
m = corrp::corr_matrix(x)
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

# Function asNamespace("asNamespace") ;
# Environment base_env = asNamespace("base") ;
# Function unlist = base_env["unlist"] ;
# Rcpp::StringVector mainvars = unlist(clu) ;
# Rcpp::StringVector nm2 = setdiff(nm,mainvars);
# Rcpp::IntegerVector mmain = match(nm, mainvars) ;
# Rcpp::IntegerVector mothers = match(nm, nm2) ;

#test with large dataset PNADc 2019

# load('dev/pnadc_dat19_i1.rda')
#
# crow = sample(1:NROW(pnadc_dat19_i1),10000)
# ccol = sample(1:NCOL(pnadc_dat19_i1),50)
# data = as.data.frame(pnadc_dat19_i1[crow,ccol])
# tictoc::tic()
# x = corrp::corrp(data,parallel = T,n.cores = 4)
# tictoc::toc()
# saveRDS(x,file = 'pnadcorrp.rds')

m - readRDS('dev/pnadcorrp.rds')

tictoc::tic()
m = corrp::corr_matrix(x)
tictoc::toc()

tictoc::tic()
acca = acca_main(m,4,1000,1500)
tictoc::toc()


