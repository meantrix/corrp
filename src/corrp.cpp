#include <Rcpp.h>
using namespace Rcpp;

RcppExport SEXP _corrp_acca_main(SEXP mSEXP, SEXP kSEXP, SEXP maxrepSEXP, SEXP maxiterSEXP);
RcppExport SEXP _corrp_silhouette_main(SEXP accaSEXP, SEXP mSEXP);
RcppExport SEXP _corrp_best_acca_sil(SEXP mSEXP, SEXP minkSEXP,SEXP maxkSEXP, SEXP maxrepSEXP, SEXP maxiterSEXP);
RcppExport SEXP _corrp_dcort_test(SEXP xSEXP, SEXP ySEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_corrp_acca_main", (DL_FUNC) &_corrp_acca_main, 4},
  {"_corrp_silhouette_main", (DL_FUNC) &_corrp_silhouette_main, 2},
  {"_corrp_best_acca_sil", (DL_FUNC) &_corrp_best_acca_sil, 5},
  {"_corrp_dcort_test", (DL_FUNC) &_corrp_dcort_test, 2},
  {NULL, NULL, 0}
};

RcppExport void R_init_corrp(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
