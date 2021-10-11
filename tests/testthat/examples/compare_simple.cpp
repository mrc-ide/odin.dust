// [[odin.dust::compare_data(observed = real_type)]]
// [[odin.dust::compare_data(another = int)]]
// [[odin.dust::compare_function]]
template <typename T>
typename T::real_type
compare(const typename T::real_type * state,
        const typename T::data_type& data,
        const typename T::internal_type internal,
        std::shared_ptr<const typename T::shared_type> shared,
        typename T::rng_state_type& rng_state) {
  // Here, 'y' refers to state[0] and 'scale' refers to shared->scale
  return (odin(y) - data.observed) / odin(scale);
}
