#include <array>
#include <Rcpp.h>

// These would be nice to make constexpr but the way that NA values
// are defined in R's include files do not allow it.
template <typename T>
inline T na_value();

template <>
inline int na_value<int>() {
  return NA_INTEGER;
}

template <>
inline double na_value<double>() {
  return NA_REAL;
}

template <typename T>
inline bool is_na(T x);

template <>
inline bool is_na(int x) {
  return Rcpp::traits::is_na<INTSXP>(x);
}

template <>
inline bool is_na(double x) {
  return Rcpp::traits::is_na<REALSXP>(x);
}

size_t object_length(Rcpp::RObject x) {
  // This is not lovely but Rcpp make it really hard to know what a
  // better way is as there seems to be no usable documentation still.
  return ::Rf_xlength(x);
}

template <typename T>
void user_check_value(T value, const char *name, T min, T max) {
  if (ISNA(value)) {
    Rcpp::stop("'%s' must not be NA", name);
  }
  if (min != na_value<T>() && value < min) {
    Rcpp::stop("Expected '%s' to be at least %g", name, (double) min);
  }
  if (max != na_value<T>() && value > max) {
    Rcpp::stop("Expected '%s' to be at most %g", name, (double) max);
  }
}

template <typename T>
void user_check_array_value(const std::vector<T>& value, const char *name,
                            T min, T max) {
  for (auto& x : value) {
    user_check_value(x, name, min, max);
  }
}


size_t user_get_array_rank(Rcpp::RObject x) {
  if (x.hasAttribute("dim")) {
    Rcpp::IntegerVector dim = x.attr("dim");
    return dim.size();
  } else {
    // This is not actually super correct
    return 1;
  }
}

template <size_t N>
void user_check_array_rank(Rcpp::RObject x, const char *name) {
  size_t rank = user_get_array_rank(x);
  if (rank != N) {
    if (N == 1) {
      Rcpp::stop("Expected a vector for '%s'", name);
    } else if (N == 2) {
      Rcpp::stop("Expected a matrix for '%s'", name);
    } else {
      Rcpp::stop("Expected an array of rank %d for '%s'", N, name);
    }
  }
}

template <size_t N>
void user_check_array_dim(Rcpp::RObject x, const char *name,
                          const std::array<int, N>& dim_expected) {
  Rcpp::IntegerVector dim = x.attr("dim");
  for (size_t i = 0; i < N; ++i) {
    if (dim[i] != dim_expected[i]) {
      Rf_error("Incorrect size of dimension %d of '%s' (expected %d)",
               i + 1, name, dim_expected[i]);
    }
  }
}

template <>
void user_check_array_dim<1>(Rcpp::RObject x, const char *name,
                             const std::array<int, 1>& dim_expected) {
  if ((int)object_length(x) != dim_expected[0]) {
    Rcpp::stop("Expected length %d value for '%s'", dim_expected[0], name);
  }
}

template <size_t N>
void user_set_array_dim(Rcpp::RObject x, const char *name,
                          std::array<int, N>& dim) {
  Rcpp::IntegerVector dim_given = x.attr("dim");
  std::copy(dim_given.begin(), dim_given.end(), dim.begin());
}

template <>
void user_set_array_dim<1>(Rcpp::RObject x, const char *name,
                           std::array<int, 1>& dim) {
  dim[0] = object_length(x);
}

template <typename T>
T user_get_scalar(Rcpp::List user, const char *name,
                  const T previous, T min, T max) {
  T ret = previous;
  if (user.containsElementNamed(name)) {
    Rcpp::RObject x = user[name];
    if (object_length(x) != 1) {
      Rcpp::stop("Expected a scalar numeric for '%s'", name);
    }
    // TODO: when we're getting out an integer this is a bit too relaxed
    if (Rcpp::is<Rcpp::NumericVector>(x)) {
      ret = Rcpp::as<T>(x);
    } else if (Rcpp::is<Rcpp::IntegerVector>(x)) {
      ret = Rcpp::as<T>(x);
    } else {
      Rcpp::stop("Expected a numeric value for %s", name);
    }
  }

  if (is_na(ret)) {
    Rcpp::stop("Expected a value for '%s'", name);
  }
  user_check_value<T>(ret, name, min, max);
  return ret;
}

template <>
float user_get_scalar<float>(Rcpp::List user, const char *name,
                             const float previous, float min, float max) {
  double value = user_get_scalar<double>(user, name, previous, min, max);
  return static_cast<float>(value);
}

template <typename T, size_t N>
std::vector<T> user_get_array_fixed(Rcpp::List user, const char *name,
                                    const std::vector<T> previous,
                                    const std::array<int, N>& dim,
                                    T min, T max) {
  if (!user.containsElementNamed(name)) {
    if (previous.size() == 0) {
      Rcpp::stop("Expected a value for '%s'", name);
    }
    return previous;
  }

  Rcpp::RObject x = user[name];

  user_check_array_rank<N>(x, name);
  user_check_array_dim<N>(x, name, dim);

  std::vector<T> ret = Rcpp::as<std::vector<T>>(x);
  user_check_array_value(ret, name, min, max);

  return ret;
}

template <typename T, size_t N>
std::vector<T> user_get_array_variable(Rcpp::List user, const char *name,
                                       std::vector<T> previous,
                                       std::array<int, N>& dim,
                                       T min, T max) {
  if (!user.containsElementNamed(name)) {
    if (previous.size() == 0) {
      Rcpp::stop("Expected a value for '%s'", name);
    }
    return previous;
  }

  Rcpp::RObject x = user[name];

  user_check_array_rank<N>(x, name);
  user_set_array_dim<N>(x, name, dim);

  std::vector<T> ret = Rcpp::as<std::vector<T>>(x);
  user_check_array_value(ret, name, min, max);

  return ret;
}

// This is sum with inclusive "from", exclusive "to", following the
// same function in odin
template <typename T>
T odin_sum1(const std::vector<T>& x, size_t from, size_t to) {
  T tot = 0.0;
  for (size_t i = from; i < to; ++i) {
    tot += x[i];
  }
  return tot;
}
