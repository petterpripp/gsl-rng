#lang racket

;(inv-cdf (normal-dist 0 1) 0.1)

(require (rename-in ffi/unsafe (-> ~>))
         ffi/unsafe/define
         ffi/unsafe/alloc)

(define-ffi-definer gslcblas (ffi-lib "libgslcblas" #:global? #t))
(define-ffi-definer gsl (ffi-lib "libgsl"  #:global? #t))

(begin-for-syntax
  (require racket/syntax syntax/parse))


(define-syntax (make-pointer stx)
  (syntax-parse stx 
    [(_ ctype)
     (with-syntax ([pointer (format-id #'ctype "_~a-pointer" (syntax->datum #'ctype))]
                   [pointer? (format-id #'ctype "~a-pointer?" (syntax->datum #'ctype))])
       #'(begin (define-cpointer-type pointer) ))]))
       ;#'(begin (define-cpointer-type pointer) (provide pointer pointer?)))]))


(make-pointer char)
(make-pointer gsl_rng_type)
(make-pointer gsl_rng)
(provide gsl_rng-pointer?)

; Turns off default gsl error handler, preventing unwanted abort of program and freeze of DrRacket.
(define _gsl_error_handler_t-pointer (_cpointer/null 'gsl_error_handler_t))
(gsl gsl_set_error_handler_off (_fun ~> _gsl_error_handler_t-pointer))
(define previous_handler (gsl_set_error_handler_off))


(define-syntax-rule (def-gsl f body)
  (begin
    (gsl f body)
    (provide f)))


(gsl gsl_rng_free (_fun _gsl_rng-pointer ~> _void)  #:wrap (deallocator))
(gsl gsl_rng_alloc (_fun _gsl_rng_type-pointer ~> _gsl_rng-pointer) #:wrap (allocator gsl_rng_free))
(gsl gsl_rng_set (_fun _gsl_rng-pointer _ulong ~> _void))

(define/contract (gsl_rng_init T [seed #f])
  (->* (symbol?) (nonnegative-integer?)  gsl_rng-pointer?)
  (begin
    (define r (gsl_rng_alloc (symbol->rng-type T)))
    (gsl_rng_set r (if seed
                       seed
                       (inexact->exact (round (* (expt 2 64) (random))))))
    r))
(provide gsl_rng_init)    


(def-gsl gsl_rng_get (_fun _gsl_rng-pointer ~> _ulong))
(def-gsl gsl_rng_uniform (_fun _gsl_rng-pointer ~> _double))
(def-gsl gsl_rng_uniform_pos (_fun _gsl_rng-pointer ~> _double))
(def-gsl gsl_rng_uniform_int (_fun _gsl_rng-pointer _ulong ~> _ulong))
(def-gsl gsl_rng_max (_fun _gsl_rng-pointer ~> _ulong))
(def-gsl gsl_rng_min (_fun _gsl_rng-pointer ~> _ulong))

(gsl gsl_rng_name (_fun _gsl_rng-pointer ~> _char-pointer))
(define/contract (rng-name r)
  (-> gsl_rng-pointer? string?)
  (cast (gsl_rng_name r) _char-pointer _string ))
(provide (rename-out [rng-name gsl_rng_name]))

(define-syntax-rule (get-rng-type id)
  (gsl id _gsl_rng_type-pointer)) 


(get-rng-type gsl_rng_borosh13)
(get-rng-type gsl_rng_coveyou)
(get-rng-type gsl_rng_cmrg)
(get-rng-type gsl_rng_fishman18)
(get-rng-type gsl_rng_fishman20)
(get-rng-type gsl_rng_fishman2x)
(get-rng-type gsl_rng_gfsr4)
(get-rng-type gsl_rng_knuthran)
(get-rng-type gsl_rng_knuthran2)
(get-rng-type gsl_rng_knuthran2002)
(get-rng-type gsl_rng_lecuyer21)
(get-rng-type gsl_rng_minstd)
(get-rng-type gsl_rng_mrg)
(get-rng-type gsl_rng_mt19937)
(get-rng-type gsl_rng_mt19937_1999)
(get-rng-type gsl_rng_mt19937_1998)
(get-rng-type gsl_rng_r250)
(get-rng-type gsl_rng_ran0)
(get-rng-type gsl_rng_ran1)
(get-rng-type gsl_rng_ran2)
(get-rng-type gsl_rng_ran3)
(get-rng-type gsl_rng_rand)
(get-rng-type gsl_rng_rand48)
(get-rng-type gsl_rng_random128_bsd)
(get-rng-type gsl_rng_random128_glibc2)
(get-rng-type gsl_rng_random128_libc5)
(get-rng-type gsl_rng_random256_bsd)
(get-rng-type gsl_rng_random256_glibc2)
(get-rng-type gsl_rng_random256_libc5)
(get-rng-type gsl_rng_random32_bsd)
(get-rng-type gsl_rng_random32_glibc2)
(get-rng-type gsl_rng_random32_libc5)
(get-rng-type gsl_rng_random64_bsd)
(get-rng-type gsl_rng_random64_glibc2)
(get-rng-type gsl_rng_random64_libc5)
(get-rng-type gsl_rng_random8_bsd)
(get-rng-type gsl_rng_random8_glibc2)
(get-rng-type gsl_rng_random8_libc5)
(get-rng-type gsl_rng_random_bsd)
(get-rng-type gsl_rng_random_glibc2)
(get-rng-type gsl_rng_random_libc5)
(get-rng-type gsl_rng_randu)
(get-rng-type gsl_rng_ranf)
(get-rng-type gsl_rng_ranlux)
(get-rng-type gsl_rng_ranlux389)
(get-rng-type gsl_rng_ranlxd1)
(get-rng-type gsl_rng_ranlxd2)
(get-rng-type gsl_rng_ranlxs0)
(get-rng-type gsl_rng_ranlxs1)
(get-rng-type gsl_rng_ranlxs2)
(get-rng-type gsl_rng_ranmar)
(get-rng-type gsl_rng_slatec)
(get-rng-type gsl_rng_taus)
(get-rng-type gsl_rng_taus2)
(get-rng-type gsl_rng_taus113)
(get-rng-type gsl_rng_transputer)
(get-rng-type gsl_rng_tt800)
(get-rng-type gsl_rng_uni)
(get-rng-type gsl_rng_uni32)
(get-rng-type gsl_rng_vax)
(get-rng-type gsl_rng_waterman14)
(get-rng-type gsl_rng_zuf)

(define (symbol->rng-type t)
  (match t
    ['borosh13 gsl_rng_borosh13]
    ['coveyou gsl_rng_coveyou]
    ['cmrg gsl_rng_cmrg]
    ['fishman18 gsl_rng_fishman18]
    ['fishman20 gsl_rng_fishman20]
    ['fishman2x gsl_rng_fishman2x]
    ['gfsr4 gsl_rng_gfsr4]
    ['knuthran gsl_rng_knuthran]
    ['knuthran2 gsl_rng_knuthran2]
    ['knuthran2002 gsl_rng_knuthran2002]
    ['lecuyer21 gsl_rng_lecuyer21]
    ['minstd gsl_rng_minstd]
    ['mrg gsl_rng_mrg]
    ['mt19937 gsl_rng_mt19937]
    ['mt19937_1999 gsl_rng_mt19937_1999]
    ['mt19937_1998 gsl_rng_mt19937_1998]
    ['r250 gsl_rng_r250]
    ['ran0 gsl_rng_ran0]
    ['ran1 gsl_rng_ran1]
    ['ran2 gsl_rng_ran2]
    ['ran3 gsl_rng_ran3]
    ['rand gsl_rng_rand]
    ['rand48 gsl_rng_rand48]
    ['random128_bsd gsl_rng_random128_bsd]
    ['random128_glibc2 gsl_rng_random128_glibc2]
    ['random128_libc5 gsl_rng_random128_libc5]
    ['random256_bsd gsl_rng_random256_bsd]
    ['random256_glibc2 gsl_rng_random256_glibc2]
    ['random256_libc5 gsl_rng_random256_libc5]
    ['random32_bsd gsl_rng_random32_bsd]
    ['random32_glibc2 gsl_rng_random32_glibc2]
    ['random32_libc5 gsl_rng_random32_libc5]
    ['random64_bsd gsl_rng_random64_bsd]
    ['random64_glibc2 gsl_rng_random64_glibc2]
    ['random64_libc5 gsl_rng_random64_libc5]
    ['random8_bsd gsl_rng_random8_bsd]
    ['random8_glibc2 gsl_rng_random8_glibc2]
    ['random8_libc5 gsl_rng_random8_libc5]
    ['random_bsd gsl_rng_random_bsd]
    ['random_glibc2 gsl_rng_random_glibc2]
    ['random_libc5 gsl_rng_random_libc5]
    ['randu gsl_rng_randu]
    ['ranf gsl_rng_ranf]
    ['ranlux gsl_rng_ranlux]
    ['ranlux389 gsl_rng_ranlux389]
    ['ranlxd1 gsl_rng_ranlxd1]
    ['ranlxd2 gsl_rng_ranlxd2]
    ['ranlxs0 gsl_rng_ranlxs0]
    ['ranlxs1 gsl_rng_ranlxs1]
    ['ranlxs2 gsl_rng_ranlxs2]
    ['ranmar gsl_rng_ranmar]
    ['slatec gsl_rng_slatec]
    ['taus gsl_rng_taus]
    ['taus2 gsl_rng_taus2]
    ['taus113 gsl_rng_taus113]
    ['transputer gsl_rng_transputer]
    ['tt800 gsl_rng_tt800]
    ['uni gsl_rng_uni]
    ['uni32 gsl_rng_uni32]
    ['vax gsl_rng_vax]
    ['waterman14 gsl_rng_waterman14]
    ['zuf gsl_rng_zuf]
    [_ (list #f t)]))
    

