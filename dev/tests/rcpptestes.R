library(Rcpp)


cppFunction("NumericVector diffs(NumericVector x, NumericVector y){
return setdiff(x,y);
}
")

evalCpp(
  "Vector<INTSXP> sample(int n, int size, bool replace = false, sugar::probs_t probs = R_NilValue, bool one_based = true);
"
)

diffs(c(1, 2, 3, 5), c(1, 2, 3, 4))
which_in2(c(1, 2, 3), c(1, 2, 2))

evalCpp("(int)10 / 3 + (int)10 % 3")
evalCpp("dentro d")
evalCpp("NumericVector v = {1,2,3};
        NumericVector y = NumericVector::create(1,2,2,2)
        v + y;
  ")
evalCpp("
  NumericVector x = NumericVector::create(1,2,3,4);
  NumericVector y = NumericVector::create(1,2,2,2,2);
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

  Rprintf(out);")


cppFunction("
    Rcpp::StringVector basic_function(Rcpp::StringVector x,Rcpp::StringVector y) {
    Rcpp::StringVector z = setdiff(x,y);
    return z;
    }")

basic_function(c("o", "o", "oi"), c("oa", "oa", "oi"))

cppFunction("void Test(){
  List L;
  List L1=List::create(12,45,22,44);
  SEXP x=Language('c',L,L1).eval();//update, add L1's elements on L's back.
  Rf_PrintValue(x) ; Rprintf( \"\n\") ;

  L = x;
  Rf_PrintValue(L) ; Rprintf( \"\n\") ;

")
