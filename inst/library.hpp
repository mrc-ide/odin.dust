template <typename real_t, typename T, typename U>
HOSTDEVICE real_t fmodr(T x, U y) {
  real_t tmp = std::fmod(static_cast<real_t>(x), static_cast<real_t>(y));
  if (tmp * y < 0) {
    tmp += y;
  }
  return tmp;
}
