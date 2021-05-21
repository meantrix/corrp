#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp ;


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



//Test Functions
int test(){
  CharacterVector C1 = StringVector::create("B", "D", "E", "F");
  CharacterVector C2 = StringVector::create("My", "Pi");
  Rcpp::List clu  =  List::create(Named("name1") = C1 , Named("name2") = C2 );
  Function asNamespace("asNamespace") ;
  Environment base_env = asNamespace("base") ;
  Function unlist = base_env["unlist"] ;
  Rcpp::StringVector nm2 = unlist(clu) ;
  Rcout << nm2 << "\n";
  Rcpp::List clu2(2) ;
  for(int j = 0; j < nm2.size(); j++){
    StringVector nm22 = Rcpp::as<StringVector>(nm2[j]);
    Rcpp::StringVector tempstr = clu[1] ;
    Rcout << "temp" << tempstr << "\n" << std::endl;
    tempstr.push_back(nm22[0]) ;
    Rcout << "temp" << tempstr << "\n" << std::endl;
    clu2[0] =  tempstr ;
    Rcout << "nm2" << nm22 << "\n" << std::endl;
  }

  return 0 ;
}

int test2(){

  NumericVector v =  NumericVector ::create(-1,-1);
  int res =  which_max(v);
  Rcout << res << std::endl;

  return 0 ;

}

// [[Rcpp::export]]
NumericMatrix m2 ( NumericMatrix m) {

  NumericMatrix ms = 1 - m ;
  return ms ;
}

// [[Rcpp::export]]
int stest(Rcpp::List acca, NumericMatrix m) {

    double a = 1.3 ;
    double b = 2.1 ;



    Rcpp::List clu = acca[acca.size()-1] ;
    int len = clu.length() ;
    Rcout << len << std::endl ;

    NumericMatrix dist = 1 - m ;
    Rcpp::IntegerVector cnames = colnames(dist) ;
    Rcpp::IntegerVector rnames = rownames(dist) ;

    Rcout << cnames << std::endl ;
    Rcout << rnames << std::endl ;

    NumericVector res_ab(len) ;

    for(int i = 0 ; i < len; i++) {
      Rcpp::IntegerVector sequ =  Rcpp::seq_len(len) -1 ;
      Rcout << "i: " << sequ << std::endl ;
      Rcout << "sequ: " << sequ << std::endl ;
      sequ.erase(i) ;
      Rcout << "sequ.e: "<< sequ << std::endl ;
      Rcout << "min: " << min(sequ) << std::endl ;
      for(int l = 0 ; l < sequ.size(); l++) {
        Rcout << "sequ.ind: " << sequ[l] << std::endl ;
      }
    }

    return 0 ;
}

////////////////////////////////////////////////////////////////////////////////
//Silhouette Alg

// S_x for each data point i
double s_x(double b, double a) {

  double res = 0 ;

  if(a > b) {

    res = 1 - a/b ;

  } else if(a == b) {

    res = 0 ;

  } else {

    res = b/a - 1 ;

  }

  return res ;
}

// [[Rcpp::export]]
double stest2(Rcpp::List acca, NumericMatrix m){

  double a = 0 ;
  double b = 0 ;

  int len = acca.size() ;
  Rcout << len << std::endl ;
  NumericMatrix dist = 1 - m ;

  colnames(dist) = colnames(m);
  rownames(dist) = rownames(m);
  NumericVector res_ab(len) ;
  Rcout << "out of loop 1: " << std::endl ;

  for(int i = 0; i < len; i++){

    Rcpp::StringVector clu_nm = acca[i] ;
    Rcpp::NumericVector clu_ab( clu_nm.length() ) ;
    Rcout << clu_nm << std::endl ;
    Rcout << "here 1 i: " << i << std::endl ;

    for(int j = 0; j < clu_nm.length(); j++){

      Rcout << "here 1 j: " << j << std::endl ;
      Rcpp::StringVector nm2 = Rcpp::as<Rcpp::StringVector>(clu_nm[j]) ;
      Rcpp::StringVector nm = setdiff(clu_nm,nm2) ;
      Rcpp::NumericMatrix m2 = subset2d(dist,nm,nm2) ;
      int row_num = m2.nrow() ;
      Rcpp::NumericVector v2 = Rcpp::colSums(m2,true)/row_num ;
      a = v2[0] ;
      Rcout << "here 2 j: " << j << std::endl ;
      Rcpp::NumericVector clu_b_inside(len-1) ;
      Rcpp::IntegerVector sequ =  Rcpp::seq_len(len) - 1 ;
      sequ.erase(i) ;
      Rcout << "sequ: " << sequ << std::endl ;
      Rcout << "here 3 j: " << j << std::endl ;

      for(int l = 0 ; l < sequ.size(); l++) {
        Rcout << "here 1 l: " << l << std::endl ;
        int other_idx = sequ[l] ;
        Rcpp::StringVector clu_nm3 = acca[other_idx] ;
        Rcpp::NumericMatrix m3 = subset2d(dist,clu_nm3,nm2) ;
        int row_num3 = m3.nrow() ;
        Rcpp::NumericVector v3 = Rcpp::colSums(m3,true)/row_num3 ;
        Rcout << "here 2 l: " << l << std::endl ;
        clu_b_inside[l] = v3[0] ;
      }
      b = min(clu_b_inside) ;
      double sx = s_x(b,a) ;

      if( internal::Rcpp_IsNA(sx) || internal::Rcpp_IsNaN(sx) ){
        sx = 0 ;
      }

      clu_ab[j] = sx ;
      Rcout << "here 4 j: " << j << std::endl ;

    }
    res_ab[i] = mean(clu_ab) ;
    Rcout << "here 2 i: " << i << std::endl ;

  }
  Rcout << "out of loop 2" << std::endl ;

  double max_ab = max(res_ab) ;
  return max_ab ;


}



