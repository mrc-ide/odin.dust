template <typename real_type, typename T, typename U>
HOSTDEVICE real_type fmodr(T x, U y) {
  real_type tmp = std::fmod(static_cast<real_type>(x),
                            static_cast<real_type>(y));
  if (tmp * y < 0) {
    tmp += y;
  }
  return tmp;
}

// These exist to support the model on the gpu, as in C++14 std::min
// and std::max are constexpr and error without --expt-relaxed-constexpr
template <typename T>
HOSTDEVICE T odin_min(T x, T y) {
  return x < y ? x : y;
}

template <typename T>
HOSTDEVICE T odin_max(T x, T y) {
  return x > y ? x : y;
}
