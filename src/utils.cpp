#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// [[Rcpp::export]]
CharacterVector csample_char( CharacterVector x,
                              int size,
                              bool replace,
                              NumericVector prob = NumericVector::create()) {
  CharacterVector ret = RcppArmadillo::sample(x, size, replace, prob) ;
  return ret ;
}




// [[Rcpp::export]]
Rcpp::List randcluster(Rcpp::NumericMatrix m,int k) {
  Rcpp::StringVector v = colnames(m) ;
  int ncol = m.ncol() ;
  int div = (int)ncol / k + (int)ncol % k ;
  Rcpp::List clu(k);
  for(int l = 0 ; l < k; l++){
        if(v.length() > div) {
          Rcpp::StringVector sv = csample_char(v,k,false);
          Rcpp::StringVector v_sv_diff = setdiff(v,sv);
          for(int j = 0 ; j < v.length(); j++) {
            for(int i = 0 ; i < v_sv_diff.length(); i++) {
              if( v[j] == v_sv_diff[i]) {
                v.erase(j);
              }
            }
          }
          clu(l) = v_sv_diff;
       } else {
          clu(l) = v;
       }
  }
return clu;
}



// [[Rcpp::export]]
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





