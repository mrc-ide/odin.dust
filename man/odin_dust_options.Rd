% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odin_dust.R
\name{odin_dust_options}
\alias{odin_dust_options}
\title{Options for odin_dust}
\usage{
odin_dust_options(
  ...,
  real_t = NULL,
  gpu = NULL,
  gpu_generate = NULL,
  options = NULL
)
}
\arguments{
\item{...}{Arguments passed to [odin::odin_options], including
`verbose`, `workdir`, `no_check_unused_equations` and
`rewrite_dims`.}

\item{real_t}{C++ type to use for real (floating point)
numbers. Defaults to \code{double}.}

\item{gpu}{**Experimental!** Generate support for running models
on a GPU. This implies `gpu_generate` but *does* require a gpu
and nvcc toolchain. Currently not supported within package code.
This argument will be passed through to [dust::dust()] and so to
enable compilation for a gpu, pass either `TRUE` or the results
of [dust::dust_cuda_options])}

\item{gpu_generate}{**Experimental** Generate gpu support
code. This does not require a gpu or nvcc toolchain, but creates
code that could be compiled for a gpu. This is primarily
intended for testing and development as the generated code will
be slower than the normal cpu version, and the compilation time
will be considerably slower. Currently not supported within
package code.}

\item{options}{An [odin::odin_options] or `odin_dust_options`
object. If given it overrides arguments; if it is already a
`odin_dust_options` object it is returned unmodified. Otherwise
it is passed through to [odin::odin_options] where it will
override arguments in `...` but respond to the `odin_dust`
specific options (`real_t`, etc)}
}
\value{
A list of options (class `odin_options`) to
  pass to `odin.dust::odin_dust`
}
\description{
Options for controlling [odin_dust]; this is a superset of
[odin::odin_options]
}
\examples{
odin.dust::odin_dust_options()
}
