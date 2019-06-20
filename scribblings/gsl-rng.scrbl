#lang scribble/manual
@(require (for-label racket))

@title{GNU GSL Random Number Generation }
by @author+email[ "Petter Pripp" "petter.pripp@yahoo.com"]
@defmodule[gsl-rng #:packages ["gsl-rng"]]

Interface to GNU GSL Random Number Generation.

Library hides memory allocation and other low level C stuff.

GNU GSL has to be installed separately.
Tested for version 2.5.

Naming of functions and keywords follow GNU GSL documentation
@url{https://www.gnu.org/software/gsl/doc/html/rng.html}

The source code is distributed under the GNU General Public License.

@section{Example}

@racketblock[             
(require gsl-rng)

(define r1 (gsl_rng_init 'mt19937 0))
(displayln (string-append "generator type: " (gsl_rng_name r1)))
(displayln (string-append "first value = " (~a (gsl_rng_get r1))))

(define r2 (gsl_rng_init  'taus 123))
(displayln (string-append "generator type: " (gsl_rng_name r2)))
(displayln (string-append "first value = " (~a (gsl_rng_get r2))))]


@#reader scribble/comment-reader
(racketblock
; generator type: mt19937
; first value = 4293858116
; generator type: taus
; first value = 2720986350
)

For more examples look at test.rkt source file.

@section{Random generators}

gsl_rng_borosh13

gsl_rng_coveyou

gsl_rng_cmrg

gsl_rng_fishman18

gsl_rng_fishman20

gsl_rng_fishman2x

gsl_rng_gfsr4

gsl_rng_knuthran

gsl_rng_knuthran2

gsl_rng_knuthran2002

gsl_rng_lecuyer21

gsl_rng_minstd

gsl_rng_mrg

gsl_rng_mt19937

gsl_rng_mt19937_1999

gsl_rng_mt19937_1998

gsl_rng_r250

gsl_rng_ran0

gsl_rng_ran1

gsl_rng_ran2

gsl_rng_ran3

gsl_rng_rand

gsl_rng_rand48

gsl_rng_random128_bsd

gsl_rng_random128_glibc2

gsl_rng_random128_libc5

gsl_rng_random256_bsd

gsl_rng_random256_glibc2

gsl_rng_random256_libc5

gsl_rng_random32_bsd

gsl_rng_random32_glibc2

gsl_rng_random32_libc5

gsl_rng_random64_bsd

gsl_rng_random64_glibc2

gsl_rng_random64_libc5

gsl_rng_random8_bsd

gsl_rng_random8_glibc2

gsl_rng_random8_libc5

gsl_rng_random_bsd

gsl_rng_random_glibc2

gsl_rng_random_libc5

gsl_rng_randu

gsl_rng_ranf

gsl_rng_ranlux

gsl_rng_ranlux389

gsl_rng_ranlxd1

gsl_rng_ranlxd2

gsl_rng_ranlxs0

gsl_rng_ranlxs1

gsl_rng_ranlxs2

gsl_rng_ranmar

gsl_rng_slatec

gsl_rng_taus

gsl_rng_taus2

gsl_rng_taus113

gsl_rng_transputer

gsl_rng_tt800

gsl_rng_uni

gsl_rng_uni32

gsl_rng_vax

gsl_rng_waterman14

gsl_rng_zuf



@section{Reference}


@defproc[(gsl_rng_init
          (T symbol?)          
          (seed (or/c #f nonnegative-integer?) #f )
          ) gsl_rng-pointer?]{
Initialize a random generator, with optional seed.
If no seed is provided, it uses Racket's random function to generate a seed.

This is a wrapper for @bold["gsl_rng_alloc"], @bold["gsl_rng_set"] and @bold["gsl_rng_free"].
}


@defproc[(gsl_rng_name
          (r gsl_rng-pointer?)                    
          ) string?]{
Name of the random generator.
}

@defproc[(gsl_rng_get
          (r gsl_rng-pointer?)                    
          ) nonnegative-integer?]{
Returns a random integer.
}

@defproc[(gsl_rng_uniform
          (r gsl_rng-pointer?)                    
          ) flonum?]{
Returns a floating point number uniformly distributed in the range [0,1).
The range includes 0.0 but excludes 1.0.}

@defproc[(gsl_rng_uniform_pos
          (r gsl_rng-pointer?)                    
          ) flonum?]{
Returns a floating point number uniformly distributed in the range [0,1),
excluding both 0.0 and 1.0.}

@defproc[(gsl_rng_uniform_int
          (r gsl_rng-pointer?)
          (n nonnegative-integer?)
          ) nonnegative-integer?]{
Returns a random integer from 0 to n-1 inclusive.}


@defproc[(gsl_rng_max
          (r gsl_rng-pointer?)                    
          ) nonnegative-integer?]{
Returns the largest value that @bold["gsl_rng_get"] can return.}

@defproc[(gsl_rng_min
          (r gsl_rng-pointer?)                    
          ) nonnegative-integer?]{
Returns the smallest value that @bold["gsl_rng_get"] can return.}


@section{Troubleshooting}
Some linux systems have precompiled package for GNU GSL library. Howeever this package can be of an older version. 
It is recommended to compile and install GNU GSL library from source.
Beware that installation directory from source can be different that from precompiled package.

If you get error:  ffi-lib: couldn't open "libgslcblas.so" (libgslcblas.so: cannot open shared object file: No such file or directory)

Solution: Edit ~/.bashrc

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib

export LD_LIBRARY_PATH
