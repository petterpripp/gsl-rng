#lang racket
 
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
       #'(begin (define-cpointer-type pointer) (provide pointer pointer?)))]))


(make-pointer char)
(make-pointer gsl_rng_type)
(make-pointer gsl_rng)


; Turns off default gsl error handler, preventing unwanted abort of program and freeze of DrRacket.
(define _gsl_error_handler_t-pointer (_cpointer/null 'gsl_error_handler_t))
;(c2gsl "gsl_error_handler_t * gsl_set_error_handler_off();")
(gsl gsl_set_error_handler_off (_fun ~> _gsl_error_handler_t-pointer))
(define previous_handler (gsl_set_error_handler_off))



(define-syntax-rule (def-gsl f body)
  (begin
    (gsl f body)
    (provide f)))


(gsl gsl_rng_free (_fun _gsl_rng-pointer ~> _void)  #:wrap (deallocator))
(gsl gsl_rng_alloc (_fun _gsl_rng_type-pointer ~> _gsl_rng-pointer) #:wrap (allocator gsl_rng_free))
(gsl gsl_rng_set (_fun _gsl_rng-pointer _ulong ~> _void))

(define/contract (rng-init T seed)
  (-> gsl_rng_type-pointer? nonnegative-integer?  gsl_rng-pointer?)
  (begin
    (define r (gsl_rng_alloc T))
    (gsl_rng_set r seed)
    r))
(provide rng-init)    


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
(provide rng-name)

(define-syntax-rule (get-rng-type id)
  (def-gsl id _gsl_rng_type-pointer)) 


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


; don't run this file for testing:
(module test racket/base)
