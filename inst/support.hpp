#include <Rcpp.h>

template <typename T>
T user_get_scalar(Rcpp::List user, const char *name,
                  T default_value, T min, T max) {
  T ret = default_value;
  bool is_na = false;
  if (user.containsElementNamed(name)) {
    Rcpp::RObject x = user[name];
    // if (x.size() != 1) {
    //   Rcpp::stop("Expected a scalar numeric for '%s'", name);
    // }
    // TODO: when we're getting out an integer this is a bit too relaxed
    if (Rcpp::is<Rcpp::NumericVector>(x)) {
      ret = Rcpp::as<T>(x);
      is_na = Rcpp::traits::is_na<REALSXP>(max);
    } else if (Rcpp::is<Rcpp::IntegerVector>(x)) {
      ret = Rcpp::as<T>(x);
      is_na = Rcpp::traits::is_na<INTSXP>(max);
    } else {
      Rcpp::stop("Expected a numeric value for %s", name);
    }
  }
  if (is_na) {
    Rcpp::stop("Expected a value for '%s'", name);
  }
  // user_check_values<T>(&ret, 1, min, max, name);
  return ret;
}
