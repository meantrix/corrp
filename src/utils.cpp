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
  Rcpp::List clu(k) ;
  for(int l = 0 ; l < k; l++){
        if(v.length() > div) {
          Rcpp::StringVector sv = csample_char(v,quo,false) ;
          Rcpp::StringVector v_sv_diff = setdiff(v,sv) ;
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

template <int RTYPE> inline Matrix<RTYPE>
Subset2D(const Matrix<RTYPE>& x, CharacterVector crows, CharacterVector ccols) {
  R_xlen_t i = 0, j = 0, rr = crows.length(), rc = ccols.length(), pos;
  Matrix<RTYPE> res(rr, rc);

  CharacterVector xrows = rownames(x) ;
  CharacterVector xcols = colnames(x) ;
  IntegerVector rows = match(crows, xrows) ;
  IntegerVector cols = match(ccols, xcols) ;

  for (; j < rc; j++) {
    // NB: match returns 1-based indices
    pos = cols[j] - 1;
    for (i = 0; i < rr; i++) {
      res(i, j) = x(rows[i] - 1, pos);
    }
  }

  rownames(res) = crows;
  colnames(res) = ccols;

  return res;
}

// [[Rcpp::export]]
//subset NumericMatrix by row and column names
//based on https://stackoverflow.com/questions/41987871/subset-numericmatrix-by-row-and-column-names-in-rcpp
NumericMatrix subset2d(NumericMatrix x, CharacterVector rows, CharacterVector cols) {
  return Subset2D(x, rows, cols);
}




// [[Rcpp::export]]
//
Rcpp::List csingle_cluster(int k , Rcpp::NumericMatrix m, Rcpp::List spl){
  Rcpp::List clu(k);
  for (int i = 0; i < k; ++i) {
    NumericMatrix my = subset2d(m,spl(i),spl(i)) ;
    NumericVector myy = Rcpp::rowSums(my,true) ;
    //NumericVector myy = as<NumericVector>(my);
    CharacterVector coln = colnames(my) ;
    int idx = which_max(myy) ;
    clu(i) = as<CharacterVector>(coln(idx)) ;
  }
  return clu ;

}



