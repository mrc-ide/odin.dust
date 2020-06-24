// [[Rcpp::export(rng = false)]]
double get_scalar_double(Rcpp::List user, double previous,
                         double min, double max) {
  return user_get_scalar<double>(user, "input", previous, min, max);
}

// [[Rcpp::export(rng = false)]]
int get_scalar_int(Rcpp::List user, int previous,
                   int min, int max) {
  return user_get_scalar<int>(user, "input", previous, min, max);
}

// [[Rcpp::export(rng = false)]]
std::vector<double> get_array_double(Rcpp::List user,
                                     std::vector<double> previous,
                                     std::vector<int> dim,
                                     double min, double max) {
  size_t rank = dim.size();
  if (rank == 1) {
    std::array<int, 1> dim_arr{dim[0]};
    return user_get_array_fixed<double, 1>(user, "input", previous, dim_arr,
                                           min, max);
  } else if (rank == 2) {
    std::array<int, 2> dim_arr{dim[0], dim[1]};
    return user_get_array_fixed<double, 2>(user, "input", previous, dim_arr,
                                           min, max);
  } else if (rank == 3) {
    std::array<int, 3> dim_arr{dim[0], dim[1], dim[2]};
    return user_get_array_fixed<double, 3>(user, "input", previous, dim_arr,
                                           min, max);
  } else {
    std::vector<double> ret(0);
    return ret;
  }
}

// [[Rcpp::export(rng = false)]]
Rcpp::List get_array_variable_double(Rcpp::List user,
                                     std::vector<double> previous,
                                     std::vector<int> dim,
                                     double min, double max) {
  std::vector<double> value;
  size_t rank = dim.size();
  if (rank == 1) {
    std::array<int, 1> dim_arr{dim[0]};
    value = user_get_array_variable<double, 1>(user, "input", previous,
                                               dim_arr, min, max);
    std::copy(dim_arr.begin(), dim_arr.end(), dim.begin());
  } else if (rank == 2) {
    std::array<int, 2> dim_arr{dim[0], dim[1]};
    value = user_get_array_variable<double, 2>(user, "input", previous,
                                               dim_arr, min, max);
    std::copy(dim_arr.begin(), dim_arr.end(), dim.begin());
  } else if (rank == 3) {
    std::array<int, 3>
      dim_arr{dim[0], dim[1], dim[2]};
    value = user_get_array_variable<double, 3>(user, "input", previous,
                                               dim_arr, min, max);
    std::copy(dim_arr.begin(), dim_arr.end(), dim.begin());
  }

  return Rcpp::List::create(value, dim);
}
