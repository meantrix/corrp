#ifndef _ACCA_H
#define _ACCA_H

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// acca_main
// ACCA main function iterates until for max_rep successive iteration no changes among clusters are found.
Rcpp::List acca_main(Rcpp::NumericMatrix m , int k , int maxrep = 2, int maxiter = 100);
RcppExport SEXP _corrp_acca_main(SEXP mSEXP, SEXP kSEXP, SEXP maxrepSEXP, SEXP maxiterSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type m(mSEXP);
  Rcpp::traits::input_parameter< int >::type k(kSEXP);
  Rcpp::traits::input_parameter< int >::type maxrep(maxrepSEXP);
  Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
  rcpp_result_gen = Rcpp::wrap(acca_main(m, k, maxrep, maxiter));
  return rcpp_result_gen;
  END_RCPP
}
// silhouette_main
// Silhouette clustering algorithm double silhouette_main(Rcpp::List acca, NumericMatrix m);
double silhouette_main(Rcpp::List acca, NumericMatrix m) ;
RcppExport SEXP _corrp_silhouette_main(SEXP accaSEXP, SEXP mSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< Rcpp::List >::type acca(accaSEXP);
  Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
  rcpp_result_gen = Rcpp::wrap(silhouette_main(acca, m));
  return rcpp_result_gen;
  END_RCPP
}


// best_acca_sil
// Find Best ACCA number of clusters k
Rcpp::List best_acca_sil(NumericMatrix m,int mink, int maxk,int maxrep = 2, int maxiter = 100);
RcppExport SEXP _corrp_best_acca_sil(SEXP mSEXP, SEXP minkSEXP,SEXP maxkSEXP, SEXP maxrepSEXP, SEXP maxiterSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type m(mSEXP);
  Rcpp::traits::input_parameter< int >::type mink(minkSEXP);
  Rcpp::traits::input_parameter< int >::type maxk(maxkSEXP);
  Rcpp::traits::input_parameter< int >::type maxrep(maxrepSEXP);
  Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
  rcpp_result_gen = Rcpp::wrap(best_acca_sil(m, mink, maxk, maxrep, maxiter));
  return rcpp_result_gen;
  END_RCPP
}



#endif // _ACCA_H
