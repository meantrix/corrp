library(Rcpp)

cppFunction("NumericVector diffs(NumericVector x, NumericVector y){
return setdiff(x,y);
}
")

diffs(c(1, 2, 3, 5), c(1, 2, 3, 4))


evalCpp("(int)10 / 3 + (int)10 % 3")

Rcpp::evalCpp('
  []() {
    NumericVector v = NumericVector::create(1, 2, 3);
    NumericVector y = NumericVector::create(1, 2, 2, 2);  
    return v + y;
  }()
')

Rcpp::evalCpp('
  // directly return the sum of two created vectors
  NumericVector::create(1, 2, 3) + NumericVector::create(1, 2, 2, 2)
')
#> [1] 2 4 5 5
Rcpp::evalCpp('
  []() {
    NumericVector x = NumericVector::create(1, 2, 3, 4);
    NumericVector y = NumericVector::create(1, 2, 2, 2, 2);
    std::vector<int> y_sort(y.size());
    std::partial_sort_copy(y.begin(), y.end(), y_sort.begin(), y_sort.end());

    int nx = x.size();
    std::vector<int> out;

    for (int i = 0; i < nx; ++i) {
      auto found = std::lower_bound(y_sort.begin(), y_sort.end(), x[i]);
      if (found != y_sort.end()) {
        out.push_back(i + 1);
      }
    }

    return out;
  }()
')


cppFunction("
    Rcpp::StringVector basic_function(Rcpp::StringVector x,Rcpp::StringVector y) {
    Rcpp::StringVector z = setdiff(x,y);
    return z;
    }")

basic_function(c("o", "o", "oi"), c("oa", "oa", "oi"))

