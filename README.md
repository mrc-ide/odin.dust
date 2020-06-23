# odin.dust <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/mrc-ide/odin.dust.svg?branch=master)](https://travis-ci.com/mrc-ide/odin.dust)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/odin.dust/badge)](https://www.codefactor.io/repository/github/mrc-ide/odin.dust)
[![codecov.io](https://codecov.io/github/mrc-ide/odin.dust/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/odin.dust?branch=master)
<!-- badges: end -->

Compile an [odin](https://mrc-ide.github.io/odin/) model to work with [dust](https://mrc-ide.github.io/dust/), so that a stochastic model can be run in parallel, for example in a particle filter such as [mcstate](https://mrc-ide.github.io/mcstate/).

This works only for models that are discrete time, and makes sense only for models that are stochastic. Within these models you cannot use `output()`, interpolation functions or delays. Not all distributions are supported by dust (at the time of writing just `runif`, `rnorm`, `rpois`, and `rbinom`).

## License

MIT © Imperial College of Science, Technology and Medicine
