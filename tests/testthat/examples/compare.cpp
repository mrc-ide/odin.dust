template <typename real_type>
real_type ldpois(real_type x, real_type lambda) {
  return x * std::log(lambda) - lambda - std::lgamma(x + 1);
}

// [[odin.dust::compare_data(incidence = real_type)]]
// [[odin.dust::compare_function]]
template <typename T>
typename T::real_type
compare(const typename T::real_type * state,
        const typename T::data_type& data,
        const typename T::internal_type internal,
        std::shared_ptr<const typename T::shared_type> shared,
        typename T::rng_state_type& rng_state) {
  typedef typename T::real_type real_type;
  const real_type incidence_modelled = odin(incidence);
  const real_type incidence_observed = data.incidence;
  const real_type lambda = incidence_modelled +
    dust::random::exponential<real_type>(rng_state, odin(exp_noise));
  return ldpois(incidence_observed, lambda);
}
