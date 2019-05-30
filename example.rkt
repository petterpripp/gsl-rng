#lang racket

(require gsl-rng)

(define r1 (rng-init gsl_rng_mt19937 0))
(displayln (string-append "generator type: " (rng-name r1)))
(displayln (string-append "first value = " (~a (gsl_rng_get r1))))

(define r2 (rng-init gsl_rng_taus 123))
(displayln (string-append "generator type: " (rng-name r2)))
(displayln (string-append "first value = " (~a (gsl_rng_get r2))))

; generator type: mt19937
; first value = 4293858116
; generator type: taus
; first value = 2720986350

; don't run this file for testing:
(module test racket/base)
