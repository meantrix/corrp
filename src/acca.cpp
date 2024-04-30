#include <RcppArmadilloExtensions/sample.h>
#include "acca.h"

using namespace Rcpp;


////////////////////////////////////////////////////////////////////////////////
//Utils

//Takes a sample of the specified size from the elements of x using either with or without replacement.
Rcpp::StringVector csample_char( Rcpp::StringVector x,
                                 int size,
                                 bool replace,
                                 Rcpp::NumericVector prob = Rcpp::NumericVector::create()) {
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

  if(x.size() == y.size()){
    Rcpp::LogicalVector r(x.size()) ;
    for( int i=0; i<x.size(); i++){
      r[i] = (compare_cha(x[i],y[i])) ;
    }
    res = as<bool>(all(r)) ;
  }

  return res;
}

// %in% operator
std::vector<int> which_in(Rcpp::IntegerVector x, Rcpp::IntegerVector y) {
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

template <int RTYPE> inline Rcpp::Matrix<RTYPE>
subset_matrix(const Rcpp::Matrix<RTYPE>& x, Rcpp::StringVector crows, Rcpp::StringVector ccols) {
  R_xlen_t i = 0, j = 0, rr = crows.length(), rc = ccols.length(), pos;
  Rcpp::Matrix<RTYPE> res(rr, rc);

  Rcpp::StringVector xrows = rownames(x) ;
  Rcpp::StringVector xcols = colnames(x) ;
  Rcpp::IntegerVector rows = match(crows, xrows) ;
  Rcpp::IntegerVector cols = match(ccols, xcols) ;

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
Rcpp::NumericMatrix subset2d(Rcpp::NumericMatrix x, Rcpp::StringVector rows, Rcpp::StringVector cols) {
  return subset_matrix(x, rows, cols);
}


////////////////////////////////////////////////////////////////////////////////
//ACCA alg

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


//get variables with maximum mean correlation per cluster
Rcpp::List csingle_acca(Rcpp::NumericMatrix m, int k , Rcpp::List spl){
  Rcpp::List clu(k);
  for (int i = 0; i < k; ++i) {
    Rcpp::NumericMatrix m2 = subset2d(m,spl[i],spl[i]) ;
    int col_num = m2.ncol();
    Rcpp::NumericVector v2 = Rcpp::rowSums(m2,true)/col_num ;
    //Rcout << "v2" << v2 << "\n" << std::endl;
    Rcpp::StringVector coln = colnames(m2) ;
    int idx = which_max(v2) ;
    clu[i] = as<Rcpp::StringVector>(coln[idx]) ;
  }
  return clu ;
}

// ACCA main function iterates until for max_rep successive iteration no changes among clusters are found.
Rcpp::List acca_main(Rcpp::NumericMatrix m , int k ,
                     int maxrep, int maxiter){

  if (maxrep > maxiter) {
    stop("maxitter must be greater than maxrep.") ;
  }

  Rcpp::List spl = crand_acca(m,k) ;
  Rcpp::List res ;
  int stp = 0 ;
  Rcpp::StringVector nm = colnames(m) ;
  Rcpp::NumericVector v (k) ;
  for(int i = 0 ; i < maxiter ; i++){

    Rcpp::List clu = csingle_acca(m,k,spl) ;
    Function asNamespace("asNamespace") ;
    Environment base_env = asNamespace("base") ;
    Function unlist = base_env["unlist"] ;
    Function identical = base_env["identical"] ;

    Rcpp::StringVector mainvars = unlist(clu) ;
    Rcpp::StringVector nm2 = setdiff(nm,mainvars) ;
    nm2 = csample_char(nm2,nm2.length(),false) ;
    for(int j = 0; j < nm2.length(); j++){
      Rcpp::StringVector nm22 = Rcpp::as<StringVector>(nm2[j]) ;
      for(int l = 0; l < clu.size(); l++){
          Rcpp::StringVector clu_nm = clu[l] ;
          NumericMatrix m2 = subset2d(m,clu_nm,nm22) ;
          int row_num = m2.nrow() ;
          NumericVector v2 = Rcpp::colSums(m2,true)/row_num ;
          double val = v2[0] ;
          if( internal::Rcpp_IsNA(val) || internal::Rcpp_IsNaN(val) ){
            val = -1 ;
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

////////////////////////////////////////////////////////////////////////////////
//Silhouette Alg

// S_x for each data point i
double s_x(double b, double a) {

  double res = 0 ;

  if(a > b) {

    res = (b - a)/a ;

  } else if(a == b) {

    res = 0 ;

  } else {

    res = (b- a)/b ;

  }

  return res ;
}

// Silhouette clustering algorithm
double silhouette_main(Rcpp::List acca, NumericMatrix m) {

  double a = 0 ;
  double b = 0 ;

  int len = acca.size() ;
  NumericMatrix dist = 1 - m ;
  colnames(dist) = colnames(m);
  rownames(dist) = rownames(m);
  NumericVector res_ab(len) ;

  for(int i = 0; i < len; i++){
    Rcpp::StringVector clu_nm = acca[i] ;
    Rcpp::NumericVector clu_ab( clu_nm.length() ) ;

    for(int j = 0; j < clu_nm.length(); j++){
      Rcpp::StringVector nm2 = Rcpp::as<Rcpp::StringVector>(clu_nm[j]) ;
      Rcpp::StringVector nm = setdiff(clu_nm,nm2) ;
      Rcpp::NumericMatrix m2 = subset2d(dist,nm,nm2) ;
      int row_num = m2.nrow() ;
      Rcpp::NumericVector v2 = Rcpp::colSums(m2,true)/row_num ;
      a = v2[0] ;

      Rcpp::IntegerVector sequ =  Rcpp::seq_len(len) - 1 ;
      sequ.erase(i) ;
      int lenb = sequ.size() ;
      Rcpp::NumericVector clu_b_inside(lenb) ;
      for(int l = 0 ; l < lenb; l++) {
        int other_idx = sequ[l];
        Rcpp::StringVector clu_nm3 = acca[other_idx] ;
        Rcpp::NumericMatrix m3 = subset2d(dist,clu_nm3,nm2) ;
        int row_num3 = m3.nrow();
        Rcpp::NumericVector v3 = Rcpp::colSums(m3,true)/row_num3 ;
        clu_b_inside[l] = v3[0] ;
      }

      b = min( na_omit(clu_b_inside) ) ;

      double sx = s_x(b,a) ;
      if( internal::Rcpp_IsNA(sx) || internal::Rcpp_IsNaN(sx) ){
        sx = 0 ;
      }
      clu_ab[j] = sx ;

    }
    res_ab[i] = mean(clu_ab) ;
  }

  double mean_ab = mean(res_ab) ;

  return mean_ab ;


}

// Find Best ACCA number of clusters k
Rcpp::List best_acca_sil(NumericMatrix m,int mink, int maxk
                ,int maxrep, int maxiter) {

  Rcpp::IntegerVector sequk =  seq(mink,maxk) ;
  Rcpp::NumericVector resval (sequk.size()) ;
  Rcpp::List acca ;

  for(int i = 0; i <sequk.size(); i ++ ) {
    int k = sequk[i] ;
    acca = acca_main(m,k,maxrep,maxiter) ;
    Rcpp::List acca_res = acca[acca.size()-1] ;
    resval[i] = silhouette_main(acca_res,m) ;

  }

  int wmax = which_max(resval) ;
  int bestk = sequk[wmax] ;
  Rcpp::List res = List::create(Named("silhouette.ave") = resval , _["k"] = sequk, _["best.k"] = bestk) ;
  return(res) ;

}



