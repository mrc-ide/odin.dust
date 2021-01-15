// [[odin.dust::compare_data]]
struct data_type {
  double observed;
};

// [[odin.dust::compare_function]]
template <typename T>
typename T::real_t compare(const typename T::real_t * state,
                           const typename T::data_t& data,
                           const typename T::internal_t internal,
                           std::shared_ptr<const typename T::shared_t> shared,
                           dust::rng_state_t<typename T::real_t>& rng_state) {
  return state[0] - data.observed;
}

/*
This still needs fixing but I think we're best off to generate it automatically by adding:
// [[odin.dust::data(observed)]]
template <>
data_t dust_data<sir2>(cpp11::list data) {
  return data_t{cpp11::as_cpp<double>(data["observed"])};
}
*/
