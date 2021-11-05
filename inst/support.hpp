#include <array>
#include <cpp11/R.hpp>
#include <cpp11/sexp.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/list.hpp>
#include <cpp11/strings.hpp>
#include <memory>
#include <vector>

template <typename T>
inline bool is_na(T x);

template <>
inline bool is_na(int x) {
  return x == NA_INTEGER;
}

template <>
inline bool is_na(double x) {
  return ISNA(x);
}

inline size_t object_length(cpp11::sexp x) {
  return ::Rf_xlength(x);
}

template <typename T>
void user_check_value(T value, const char *name, T min, T max) {
  if (is_na(value)) {
    cpp11::stop("'%s' must not be NA", name);
  }
  if (!is_na(min) && value < min) {
    cpp11::stop("Expected '%s' to be at least %g", name, (double) min);
  }
  if (!is_na(max) && value > max) {
    cpp11::stop("Expected '%s' to be at most %g", name, (double) max);
  }
}

template <typename T>
void user_check_array_value(const std::vector<T>& value, const char *name,
                            T min, T max) {
  for (auto& x : value) {
    user_check_value(x, name, min, max);
  }
}

inline size_t user_get_array_rank(cpp11::sexp x) {
  if (!::Rf_isArray(x)) {
    return 1;
  } else {
    cpp11::integers dim = cpp11::as_cpp<cpp11::integers>(x.attr("dim"));
    return dim.size();
  }
}

template <size_t N>
void user_check_array_rank(cpp11::sexp x, const char *name) {
  size_t rank = user_get_array_rank(x);
  if (rank != N) {
    if (N == 1) {
      cpp11::stop("Expected a vector for '%s'", name);
    } else if (N == 2) {
      cpp11::stop("Expected a matrix for '%s'", name);
    } else {
      cpp11::stop("Expected an array of rank %d for '%s'", N, name);
    }
  }
}

template <size_t N>
void user_check_array_dim(cpp11::sexp x, const char *name,
                          const std::array<int, N>& dim_expected) {
  cpp11::integers dim = cpp11::as_cpp<cpp11::integers>(x.attr("dim"));
  for (size_t i = 0; i < N; ++i) {
    if (dim[(int)i] != dim_expected[i]) {
      Rf_error("Incorrect size of dimension %d of '%s' (expected %d)",
               i + 1, name, dim_expected[i]);
    }
  }
}

template <>
inline void user_check_array_dim<1>(cpp11::sexp x, const char *name,
                                    const std::array<int, 1>& dim_expected) {
  if ((int)object_length(x) != dim_expected[0]) {
    cpp11::stop("Expected length %d value for '%s'", dim_expected[0], name);
  }
}

template <size_t N>
void user_set_array_dim(cpp11::sexp x, const char *name,
                        std::array<int, N>& dim) {
  cpp11::integers dim_given = cpp11::as_cpp<cpp11::integers>(x.attr("dim"));
  std::copy(dim_given.begin(), dim_given.end(), dim.begin());
}

template <>
inline void user_set_array_dim<1>(cpp11::sexp x, const char *name,
                                  std::array<int, 1>& dim) {
  dim[0] = object_length(x);
}

template <typename T>
T user_get_scalar(cpp11::list user, const char *name,
                  const T previous, T min, T max) {
  T ret = previous;
  cpp11::sexp x = user[name];
  if (x != R_NilValue) {
    if (object_length(x) != 1) {
      cpp11::stop("Expected a scalar numeric for '%s'", name);
    }
    // TODO: when we're getting out an integer this is a bit too relaxed
    if (TYPEOF(x) == REALSXP) {
      ret = cpp11::as_cpp<T>(x);
    } else if (TYPEOF(x) == INTSXP) {
      ret = cpp11::as_cpp<T>(x);
    } else {
      cpp11::stop("Expected a numeric value for %s", name);
    }
  }

  if (is_na(ret)) {
    cpp11::stop("Expected a value for '%s'", name);
  }
  user_check_value<T>(ret, name, min, max);
  return ret;
}

template <>
inline float user_get_scalar<float>(cpp11::list user, const char *name,
                                    const float previous, float min, float max) {
  double value = user_get_scalar<double>(user, name, previous, min, max);
  return static_cast<float>(value);
}

template <typename T>
std::vector<T> user_get_array_value(cpp11::sexp x, const char * name,
                                    T min, T max) {
  std::vector<T> ret = cpp11::as_cpp<std::vector<T>>(x);
  user_check_array_value<T>(ret, name, min, max);
  return ret;
}

template <typename T, size_t N>
std::vector<T> user_get_array_fixed(cpp11::list user, const char *name,
                                    const std::vector<T> previous,
                                    const std::array<int, N>& dim,
                                    T min, T max) {
  cpp11::sexp x = user[name];
  if (x == R_NilValue) {
    if (previous.size() == 0) {
      cpp11::stop("Expected a value for '%s'", name);
    }
    return previous;
  }

  user_check_array_rank<N>(x, name);
  user_check_array_dim<N>(x, name, dim);

  return user_get_array_value<T>(x, name, min, max);
}

template <typename T, size_t N>
std::vector<T> user_get_array_variable(cpp11::list user, const char *name,
                                       std::vector<T> previous,
                                       std::array<int, N>& dim,
                                       T min, T max) {
  cpp11::sexp x = user[name];
  if (x == R_NilValue) {
    if (previous.size() == 0) {
      cpp11::stop("Expected a value for '%s'", name);
    }
    return previous;
  }

  user_check_array_rank<N>(x, name);
  user_set_array_dim<N>(x, name, dim);

  return user_get_array_value<T>(x, name, min, max);
}

template <>
inline std::vector<float> user_get_array_value(cpp11::sexp x, const char * name,
                                               float min, float max) {
  // NOTE: possible under/overflow here for min/max because we've
  // downcast this.
  std::vector<double> value = user_get_array_value<double>(x, name, min, max);
  std::vector<float> ret(value.size());
  std::copy(value.begin(), value.end(), ret.begin());
  return ret;
}

// This is sum with inclusive "from", exclusive "to", following the
// same function in odin
template <typename real_type, typename container>
__host__ __device__
real_type odin_sum1(const container x, size_t from, size_t to) {
  real_type tot = 0.0;
  for (size_t i = from; i < to; ++i) {
    tot += x[i];
  }
  return tot;
}

inline cpp11::writable::integers integer_sequence(size_t from, size_t len) {
  cpp11::writable::integers ret(len);
  int* data = INTEGER(ret);
  for (size_t i = 0, j = from; i < len; ++i, ++j) {
    data[i] = j;
  }
  return ret;
}
