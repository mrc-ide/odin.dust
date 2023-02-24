# odin.dust 0.3.1

* Support for odin's new `print()` based debugging

# odin.dust 0.3.0

* Complete support for continuous time models; these can contain both ODEs and discretely-timed stochastic updates (using recent dust)

# odin.dust 0.2.24

* Add support for `rgamma` and `rnbinom`, using new dust

# odin.dust 0.2.15

* Avoid stack overflow when generating very long expressions

# odin.dust 0.2.5

* More efficient GPU code generation (#75)

# odin.dust 0.2.4

* Support `as.numeric()` in odin code (since odin 1.1.10) (#43)

# odin.dust 0.2.3

* Can pass more options to `odin::odin_options` with better support in `odin.dust::odin_dust_options`, including odin's new `rewrite_dims` argument (#73)

# odin.dust 0.2.1

* `data_t` declarations can use `real_t` rather than `double` (#57)

# odin.dust 0.2.0

* GPU support (#66)

# odin.dust 0.1.15

* Start implementing dust gpu support by generating interleaved update function, optionally (#37)

# odin.dust 0.1.11

* Support for dust `compare` functions (#51)

# odin.dust 0.1.10

* Implement modulo (`%%`) operator, which uses `std::fmod`

# odin.dust 0.1.8

* Correct age-structured model in package vignette

# odin.dust 0.1.7

* Separate internal data into mutable and non-mutable components (#45)

# odin.dust 0.1.6

* Support for vectors of floats (rather than doubles) (#6)

# odin.dust 0.1.5

* Support `as.integer()` in odin code (since odin 1.0.7) (#43)

# odin.dust 0.1.4

* Add support for `config(include) <- file.cpp` for including custom C++ code to extend the library support (#39)

# odin.dust 0.1.0

* Add support for odin-like `coef()`, which returns information about `user` parameters (#32)
