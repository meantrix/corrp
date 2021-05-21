#include <Rcpp.h>
using namespace Rcpp;

RcppExport SEXP _corrp_acca_main(SEXP mSEXP, SEXP kSEXP, SEXP maxrepSEXP, SEXP maxiterSEXP);
RcppExport SEXP _corrp_silhouette_main(SEXP accaSEXP, SEXP mSEXP);


static const R_CallMethodDef CallEntries[] = {
  {"_corrp_acca_main", (DL_FUNC) &_corrp_acca_main, 4},
  {"_corrp_silhouette_main", (DL_FUNC) &_corrp_silhouette_main, 2},
  {"_corrp_best_acca_sil", (DL_FUNC) &_corrp_silhouette_main, 5},
  {NULL, NULL, 0}
};

RcppExport void R_init_corrp(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
