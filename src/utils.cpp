#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// [[Rcpp::export]]
//Takes a sample of the specified size from the elements of x using either with or without replacement.
CharacterVector csample_char( CharacterVector x,
                              int size,
                              bool replace,
                              NumericVector prob = NumericVector::create()) {
  CharacterVector ret = RcppArmadillo::sample(x, size, replace, prob) ;
  return ret ;
}


// [[Rcpp::export]]
// Group cmatrix into k uniform random cluster
Rcpp::List crand_cluster(Rcpp::NumericMatrix m,int k) {
  Rcpp::StringVector v = colnames(m) ;
  int ncol = m.ncol() ;
  int quo = (int)ncol / k ;
  int div = quo + (int)ncol % k ;
  Rcpp::List clu(k);
  for(int l = 0 ; l < k; l++){
        if(v.length() > div) {
          Rcpp::StringVector sv = csample_char(v,quo,false);
          Rcpp::StringVector v_sv_diff = setdiff(v,sv);
          v = v_sv_diff ;
          clu(l) = sv ;
        } else {
          clu(l) = v ;
       }
  }
return clu ;
}


// [[Rcpp::export]]
// %in% operator
std::vector<int> which_in(IntegerVector x, IntegerVector y) {
  std::vector<int> y_sort(y.size());
  std::partial_sort_copy (y.begin(), y.end(), y_sort.begin(), y_sort.end());

  int nx = x.size();
  std::vector<int> out;

  for (int i = 0; i < nx; ++i) {
    std::vector<int>::iterator found =
      lower_bound(y_sort.begin(), y_sort.end(), x[i]);
    if (found != y_sort.end()) {
      out.push_back(i + 1);
    }
  }
  return out;
}





