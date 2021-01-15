template <typename real_t>
real_t ldpois(real_t x, real_t lambda) {
  return x * std::log(lambda) - lambda - std::lgamma(x + 1);
}

// This would work, but is (1) really ugly and (2) sensitive to things
// like which parameters are shared or not. I don't think that there's
// much we can do about that though given the number of parameters
// that need to be copied around.
// [[odin.dust::compare_function]]
template <typename real_t, typename T>
real_t compare(const real_t * state, const data_t& data,
               const typename T::internal_t internal,
               std::shared_ptr<const typename T::shared_t> shared,
               dust::rng_state_t<real_t>& rng_state) {
  const real_t incidence_modelled = state[4];
  const real_t incidence_observed = data.incidence;
  const real_t lambda = incidence_modelled +
    dust::distr::rexp(rng_state, shared.exp_noise);
  return ldpois(incidence_observed, lambda);
}

// We can make this considerably nicer if we're willing to process
// just a *little* bit, but we can come and add that later. It's
// possible that we could totally automate this by providing a genuine
// C++ template or macro function.
template <typename real_t, typename T>
real_t compare(const real_t * state, const data_t& data,
               const typename T::internal_t internal,
               std::shared_ptr<const typename T::shared_t> shared,
               dust::rng_state_t<real_t>& rng_state) {
  const real_t incidence_modelled = ODIN(incidence);
  const real_t incidence_observed = data.incidence;
  const real_t lambda = incidence_modelled +
    dust::distr::rexp(rng_state, ODIN(exp_noise));
  return ldpois(incidence_observed, lambda);
}

// This is the last trick; we need to know what the model name here is
// in order to get this right but we could typedef that in easily
// enough so that this works (otherwise data_t is not defined)
// [[odin.dust::compare_data]]
struct data_t {
  double incidence;
}

template <>
data_t dust_data<sir2>(cpp11::list data) {
  return data_t{cpp11::as_cpp<double>(data["incidence"])};
}
