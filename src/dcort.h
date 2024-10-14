#ifndef _DCORT_H
#define _DCORT_H

#include <Rcpp.h>

using namespace Rcpp;


// dcort_test_Rcpp
List dcort_test(const NumericMatrix& x, const NumericMatrix& y);
RcppExport SEXP _corrp_dcort_test(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(dcort_test(x, y));
    return rcpp_result_gen;
END_RCPP
}


#endif // _DCORT_H
