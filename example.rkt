#lang racket

(require gsl-rng)

(define r1 (gsl_rng_init 'mt19937 0))
(displayln (string-append "generator type: " (gsl_rng_name r1)))
(displayln (string-append "first value = " (~a (gsl_rng_get r1))))

(define r2 (gsl_rng_init  'taus 123))
(displayln (string-append "generator type: " (gsl_rng_name r2)))
(displayln (string-append "first value = " (~a (gsl_rng_get r2))))

; generator type: mt19937
; first value = 4293858116
; generator type: taus
; first value = 2720986350

; don't run this file for testing:
(module test racket/base)
