/*
template <typename T>
class vector_offset {
  vector_offset(std::vector<T>& data, size_t offset) :
    data_(data), offset_(offset) {
  }
  T& operator[](size_t i) const {
    return data_[i + offset];
  }
private:
  std::vector<T>& data_;
  size_t offset_;
};
*/


template<typename T>
T user_get_scalar(Rcpp::List user, const char *name,
                  T default_value, T min, T max) {
  T ret = default_value;
  if (user.containsElementNamed(name)) {
    Rcpp::Object el = user[name];
    if (el.size() != 1) {
      Rcpp::stop("Expected a scalar numeric for '%s'", name);
    }
    // TODO: when we're getting out an integer this is a bit too relaxed
    if (Rcpp::is<Rcpp::NumericVector>(x) || Rcpp::is<Rcpp::IntegerVector>(x)) {
      ret = Rcpp::as<T>(x);
    } else {
      Rcpp::stop("Expected a numeric value for %s", name);
    }
  }
  if (Rcpp::is_na(ret)) {
    Rf_error("Expected a value for '%s'", name);
  }
  // user_check_values<T>(&ret, 1, min, max, name);
  return ret;
}
