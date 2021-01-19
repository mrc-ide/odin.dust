template <typename real_t>
real_t ldpois(real_t x, real_t lambda) {
  return x * std::log(lambda) - lambda - std::lgamma(x + 1);
}

// [[odin.dust::compare_data(incidence = double)]]
// [[odin.dust::compare_function]]
template <typename T>
typename T::real_t compare(const typename T::real_t * state,
                           const typename T::data_t& data,
                           const typename T::internal_t internal,
                           std::shared_ptr<const typename T::shared_t> shared,
                           dust::rng_state_t<typename T::real_t>& rng_state) {
  typedef typename T::real_t real_t;
  const real_t incidence_modelled = odin(incidence);
  const real_t incidence_observed = data.incidence;
  const real_t lambda = incidence_modelled +
    dust::distr::rexp(rng_state, odin(exp_noise));
  return ldpois(incidence_observed, lambda);
}
