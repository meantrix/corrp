#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


//Takes a sample of the specified size from the elements of x using either with or without replacement.
Rcpp::StringVector csample_char( Rcpp::StringVector x,
                              int size,
                              bool replace,
                              NumericVector prob = NumericVector::create()) {
  Rcpp::StringVector ret = RcppArmadillo::sample(x, size, replace, prob) ;
  return ret ;
}

//Compare Two Rcpp::StringVectors
bool compare_cha( Rcpp::StringVector x, Rcpp::StringVector y){

  bool res = false ;

  if(x.size() == y.size()){
    Rcpp::LogicalVector r(x.size()) ;
    for( int i=0; i<x.size(); i++){
      r[i] = (x[i] == y[i]) ;
    }
    res = as<bool>(all(r)) ;
  }
  return res ;
}

//Compare Two Lists with Rcpp::StringVectors
bool compare_list_cha( Rcpp::List x, Rcpp::List y){

  bool res = false ;

  if(x.length() == y.length()){
      Rcpp::LogicalVector r(x.length()) ;
      for( int i=0; i<x.length(); i++){
        r[i] = (compare_cha(x[i],y[i])) ;
      }
      res = as<bool>(all(r)) ;
  }

  return res;
}

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
subset_matrix(const Matrix<RTYPE>& x, Rcpp::StringVector crows, Rcpp::StringVector ccols) {
  R_xlen_t i = 0, j = 0, rr = crows.length(), rc = ccols.length(), pos;
  Matrix<RTYPE> res(rr, rc);

  Rcpp::StringVector xrows = rownames(x) ;
  Rcpp::StringVector xcols = colnames(x) ;
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



//subset NumericMatrix by row and column names
//based on https://stackoverflow.com/questions/41987871/subset-numericmatrix-by-row-and-column-names-in-rcpp
NumericMatrix subset2d(NumericMatrix x, Rcpp::StringVector rows, Rcpp::StringVector cols) {
  return subset_matrix(x, rows, cols);
}


// [[Rcpp::export]]
// Group cmatrix into k uniform random clusters
Rcpp::List crand_acca(Rcpp::NumericMatrix m,int k) {
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
      clu[l] = sv ;
    } else {
      clu[l] = v ;
    }
  }
  return clu ;
}


// [[Rcpp::export]]
//get variables with maximum mean correlation per cluster
Rcpp::List csingle_acca(Rcpp::NumericMatrix m, int k , Rcpp::List spl){
  Rcpp::List clu(k);
  for (int i = 0; i < k; ++i) {
    NumericMatrix m2 = subset2d(m,spl[i],spl[i]) ;
    int col_num = m2.ncol();
    NumericVector v2 = Rcpp::rowSums(m2,true)/col_num ;
    //Rcout << "v2" << v2 << "\n" << std::endl;
    Rcpp::StringVector coln = colnames(m2) ;
    int idx = which_max(v2) ;
    clu[i] = as<Rcpp::StringVector>(coln[idx]) ;
  }
  return clu ;
}

// [[Rcpp::export]]
// ACCA main function iterates until for max_rep successive iteration no changes among clusters are found.
Rcpp::List acca_main(NumericMatrix m , int k ,
                     int maxrep = 2, int maxiter = 100){

  if (maxrep > maxiter) {
    stop("maxitter must be greater than maxrep.") ;
  }

  Rcpp::List spl = crand_acca(m,k) ;
  Rcpp::List res ;
  int stp = 0 ;
  Rcpp::StringVector nm = colnames(m) ;
  NumericVector v (k) ;
  for(int i = 0 ; i < maxiter ; i++){

    Rcpp::List clu = csingle_acca(m,k,spl) ;
    Function asNamespace("asNamespace") ;
    Environment base_env = asNamespace("base") ;
    Function unlist = base_env["unlist"] ;
    Function identical = base_env["identical"] ;

    Rcpp::StringVector mainvars = unlist(clu) ;
    Rcpp::StringVector nm2 = setdiff(nm,mainvars) ;

    for(int j = 0; j < nm2.size(); j++){
      Rcpp::StringVector nm22 = Rcpp::as<StringVector>(nm2[j]) ;
      for(int l = 0; l < clu.length(); l++){
          Rcpp::StringVector clu_nm = clu[l] ;
          NumericMatrix m2 = subset2d(m,clu_nm,nm22) ;
          int row_num = m2.nrow();
          NumericVector v2 = Rcpp::colSums(m2,true)/row_num ;
          double val = v2[0] ;
          if( internal::Rcpp_IsNA(val) || internal::Rcpp_IsNaN(val) ){
            val = -2 ;
          }
          v[l] = val ;
      }
      //Rcout << v << "\n" << std::endl ;
      int clu_idx  = which_max(v) ;
      Rcpp::StringVector tempstr = clu[clu_idx] ;
      tempstr.push_back(nm22[0]) ;
      clu[clu_idx] = tempstr ;
    }

    spl = clu ;
    res.push_back(spl) ;
    int h = i -1 ;

    if( i > 0 && identical(res[i],res[h]) ) {
      stp++ ;
    }
    if( stp > maxrep ){
      break ;
    }

  }
return res  ;
}


