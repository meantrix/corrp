#include <Rcpp.h>
using namespace Rcpp;
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

    double a = 0 ;
    double b = 0 ;

    Rcpp::List clu = acca[acca.length()] ;
    int len = clu.length() ;
    Rcout << len << std::endl ;

    NumericMatrix dist = 1 - m ;
    NumericVector res_ab(len) ;

    for(int i = 0 ; i < len; i++) {
      Rcpp::IntegerVector sequ =  Rcpp::seq_len(len) ;
      Rcout << "i: " << sequ << std::endl ;
      Rcout << "sequ: " << sequ << std::endl ;
      sequ.erase(i) ;
      Rcout << "sequ.e: "<< sequ << std::endl ;
      for(int l = 0 ; l < sequ.size(); l++) {
        Rcout << "sequ.ind: " << sequ[l] << std::endl ;
      }
    }

    return 0 ;
}
