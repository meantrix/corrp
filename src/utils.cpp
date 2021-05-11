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


//Comapre Two CharacterVectors
bool compare_cha( CharacterVector x, CharacterVector y){
  Rcpp::LogicalVector r(x.size());
  for( int i=0; i<x.size(); i++){
    r[i] = (x[i] == y[i]);
  }
  return(all(r));
}

//Comapre Two Lists with only CharacterVectors
bool compare_list_cha( Rcpp::List x, Rcpp::List y){
  Rcpp::LogicalVector r(x.length());
  for( int i=0; i<x.length(); i++){
    r[i] = (compare_cha(x(i),y(i)));
  }
  return(all(r));
}




// [[Rcpp::export]]
// Group cmatrix into k uniform random clusters
Rcpp::List crand_acca(Rcpp::NumericMatrix m,int k) {
  CharacterVector v = colnames(m) ;
  int ncol = m.ncol() ;
  int quo = (int)ncol / k ;
  int div = quo + (int)ncol % k ;
  Rcpp::List clu(k) ;
  for(int l = 0 ; l < k; l++){
        if(v.length() > div) {
          CharacterVector sv = csample_char(v,quo,false) ;
          CharacterVector v_sv_diff = setdiff(v,sv) ;
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
subset_matrix(const Matrix<RTYPE>& x, CharacterVector crows, CharacterVector ccols) {
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
  return subset_matrix(x, rows, cols);
}


// [[Rcpp::export]]
//get variable with maximmun mean correlation per cluster
Rcpp::List csingle_acca(Rcpp::NumericMatrix m, int k , Rcpp::List spl){
  Rcpp::List clu(k);
  for (int i = 0; i < k; ++i) {
    NumericMatrix my = subset2d(m,spl(i),spl(i)) ;
    NumericVector myy = Rcpp::rowMeans(my,true) ;
    CharacterVector coln = colnames(my) ;
    int idx = which_max(myy) ;
    clu(i) = as<CharacterVector>(coln(idx)) ;
  }
  return clu ;

}



// [[Rcpp::export]]
//ACCA main function iterates until for max_rep successive iteration no changes among clusters are found.
List acca_main(NumericMatrix m , int k ,
                     int max_rep = NA_INTEGER,int maxiter = 100){

  if (Rcpp::internal::Rcpp_IsNA(max_rep)) {
    max_rep = 2 ;
  }

  Rcpp::List spl = crand_acca(m,k) ;

  Rcpp::List res;
  int stp = 0;
  CharacterVector nm = colnames(m);

  for(int i = 0 ; i < maxiter ; i++){

    Rcpp::List clu = csingle_acca(m,k,spl) ;
    NumericVector v (k) ;
    Function asNamespace("asNamespace") ;
    Environment base_env = asNamespace("base") ;
    Function unlist = base_env["unlist"] ;
    CharacterVector mainvars = unlist(clu) ;
    CharacterVector nm2 = setdiff(nm,mainvars);
    Rcpp::IntegerVector mothers = match(nm, nm2) ;


    for(int j = 0; j < nm2.size(); j++){
      for(int l = 0; l < clu.length(); l++){
          CharacterVector clu_nm = clu(l) ;
          CharacterVector nm22 = as<CharacterVector>(nm2(j)) ;
          NumericMatrix my = subset2d(m,clu_nm,nm22) ;
          double val  = as<double>(colMeans(my,true)) ;
          v(l) = val ;
      }
      int clu_idx  = which_max(v) ;
      CharacterVector tempstr = clu(clu_idx) ;
      tempstr.push_back(mothers(j)) ;
      clu(clu_idx) = tempstr ;
    }
    res.push_back(clu) ;

    if( i > 0 && compare_list_cha( res(i),res(i-1) ) ) {
        stp = stp +1 ;
    }

    if(stp > 1 ){
      break;
    }

  }
return(res) ;
}



