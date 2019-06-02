#lang info

(define collection "gsl-rng")
(define deps '("base"
               "scribble-lib"
               "rackunit-lib"
               "racket-doc"))
(define scribblings '(("scribblings/gsl-rng.scrbl" ())))
(define pkg-desc "Binding to gnu gsl random number generation")
(define pkg-authors '(Petter Pripp))
(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))
