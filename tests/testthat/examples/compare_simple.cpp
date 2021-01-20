// [[odin.dust::compare_data(observed = double)]]
// [[odin.dust::compare_data(another = int)]]
// [[odin.dust::compare_function]]
template <typename T>
typename T::real_t compare(const typename T::real_t * state,
                           const typename T::data_t& data,
                           const typename T::internal_t internal,
                           std::shared_ptr<const typename T::shared_t> shared,
                           dust::rng_state_t<typename T::real_t>& rng_state) {
  // Here, odin(y) refers to state[0] and scale refers to shared->scale
  return (odin(y) - data.observed) / odin(scale);
}
