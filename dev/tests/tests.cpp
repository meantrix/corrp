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
  
  for(int i=0; i<clu2.length(); ++i){
    Rprintf("the value of clu2[%i] : %f \n", i, clu2[i]);
  }
  
  
  return 0 ;
}

int test2(){
  
  NumericVector v =  NumericVector ::create(-1,-1);
  int res =  which_max(v);
  Rcout << res << std::endl;
  
  return 0 ;
  
}

