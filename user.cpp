#include <array>
#include <Rcpp.h>

// Control over user parameters; this will end up in support.hpp, and
// get pulled into some code for direct testing.

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
                          const std::array<size_t, N>& dim_expected) {
  Rcpp::IntegerVector dim = x.attr("dim");
  for (size_t i = 0; i < N; ++i) {
    if ((size_t) dim[i] != dim_expected[i]) {
      Rf_error("Incorrect size of dimension %d of '%s' (expected %d)",
               i + 1, name, dim_expected[i]);
    }
  }
}

template <>
void user_check_array_dim<1>(Rcpp::RObject x, const char *name,
                             const std::array<size_t, 1>& dim_expected) {
  if (object_length(x) != dim_expected[0]) {
    Rcpp::stop("Expected length %d value for '%s'", dim_expected[0], name);
  }
}

template <size_t N>
void user_set_array_dim(Rcpp::RObject x, const char *name,
                          std::array<size_t, N>& dim) {
  Rcpp::IntegerVector dim_given = x.attr("dim");
  std::copy(dim_given.begin(), dim_given.end(), dim.begin());
}

template <>
void user_set_array_dim<1>(Rcpp::RObject x, const char *name,
                           std::array<size_t, 1>& dim) {
  dim[0] = object_length(x);
}

template <typename T>
T user_get_scalar(Rcpp::List user, const char *name,
                  T previous, T min, T max) {
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

template <typename T, size_t N>
std::vector<T> user_get_array_fixed(Rcpp::List user, const char *name,
                                    std::vector<T> previous,
                                    const std::array<size_t, N>& dim,
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
                                       std::array<size_t, N>& dim,
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


// ----- below here is just use for testing and will be removed

// [[Rcpp::export(rng = false)]]
double test_user_get_scalar_double(Rcpp::List user, double previous,
                                   double min, double max) {
  return user_get_scalar<double>(user, "input", previous, min, max);
}

// [[Rcpp::export(rng = false)]]
int test_user_get_scalar_int(Rcpp::List user, int previous,
                             int min, int max) {
  return user_get_scalar<int>(user, "input", previous, min, max);
}

// [[Rcpp::export(rng = false)]]
std::vector<double> test_user_get_array_double(Rcpp::List user,
                                               std::vector<double> previous,
                                               std::vector<int> dim,
                                               double min, double max) {
  size_t rank = dim.size();
  if (rank == 1) {
    std::array<size_t, 1> dim_arr{(size_t) dim[0]};
    return user_get_array_fixed<double, 1>(user, "input", previous, dim_arr,
                                           min, max);
  } else if (rank == 2) {
    std::array<size_t, 2> dim_arr{(size_t) dim[0], (size_t) dim[1]};
    return user_get_array_fixed<double, 2>(user, "input", previous, dim_arr,
                                           min, max);
  } else if (rank == 3) {
    std::array<size_t, 3> dim_arr
      {(size_t) dim[0], (size_t) dim[1], (size_t) dim[2]};
    return user_get_array_fixed<double, 3>(user, "input", previous, dim_arr,
                                           min, max);
  } else {
    std::vector<double> ret(0);
    return ret;
  }
}

// [[Rcpp::export(rng = false)]]
Rcpp::List test_user_get_array_variable_double(Rcpp::List user,
                                               std::vector<double> previous,
                                               std::vector<int> dim,
                                               double min, double max) {
  std::vector<double> value;
  size_t rank = dim.size();
  if (rank == 1) {
    std::array<size_t, 1> dim_arr{(size_t) dim[0]};
    value = user_get_array_variable<double, 1>(user, "input", previous,
                                               dim_arr, min, max);
    std::copy(dim_arr.begin(), dim_arr.end(), dim.begin());
  } else if (rank == 2) {
    std::array<size_t, 2> dim_arr{(size_t) dim[0], (size_t) dim[1]};
    value = user_get_array_variable<double, 2>(user, "input", previous,
                                               dim_arr, min, max);
    std::copy(dim_arr.begin(), dim_arr.end(), dim.begin());
  } else if (rank == 3) {
    std::array<size_t, 3>
      dim_arr{(size_t) dim[0], (size_t) dim[1], (size_t) dim[2]};
    value = user_get_array_variable<double, 3>(user, "input", previous,
                                               dim_arr, min, max);
    std::copy(dim_arr.begin(), dim_arr.end(), dim.begin());
  }

  return Rcpp::List::create(value, dim);
}
