#ACCA testes C++ functions
library(corrp)
x = corrp::corrp(airquality)
m = corrp::corr_matrix(x)
nm = colnames(m)
spl = crand_acca(m,2)
m1 = subset2d(m,spl[[1]],spl[[1]])
single1 = csingle_acca(m,2,spl)
subset2d(m, colnames(m) , colnames(m)[1] )
a = acca_main(m,2,2,20)
l1 = a[15]
l2 = a[20]
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


Rcpp::evalCpp('CharacterVector x = CharacterVector::create("a","b","c","a","b","c","a");
               Rprintf("print",x);')



