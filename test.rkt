#lang racket

(require ffi/unsafe
         "rnd.rkt"
         rackunit
         rackunit/text-ui)
;(require infix)


(define-check (rng_test T seed n result)
  (begin
    
    (define r (gsl_rng_alloc T))
    
    (when (not (= seed 0))
      (gsl_rng_set r seed))

    (define k    
      (foldl (lambda (i acc) (gsl_rng_get r)) 0 (range n)))

    (define rng_name (cast (gsl_rng_name r) _char-pointer _string ))

    (gsl_rng_free r)
    
    (when (not (= k result))        
      (with-check-info (['rng (string-info rng_name)]
                        ['actual k]
                        ['expected result])
        (fail-check)))))
    


(define rng-tests
  (test-suite
   "Tests for rng"
  
   ;(gsl_rng_env_setup)
   
   ; specific tests of known results for 10000 iterations with seed = 1 
   (rng_test gsl_rng_rand 1 10000 1910041713)
   (rng_test gsl_rng_randu 1 10000 1623524161)
   (rng_test gsl_rng_cmrg 1 10000 719452880)
   (rng_test gsl_rng_minstd 1 10000 1043618065)
   (rng_test gsl_rng_mrg 1 10000 2064828650)
   (rng_test gsl_rng_taus 1 10000 2733957125)
   (rng_test gsl_rng_taus2 1 10000 2733957125)
   (rng_test gsl_rng_taus113 1 1000 1925420673)
   (rng_test gsl_rng_transputer 1 10000 1244127297)
   (rng_test gsl_rng_vax 1 10000 3051034865)   

   ; Borosh13 test value from PARI: (1812433253^10000)%(2^32) 
   (rng_test gsl_rng_borosh13 1 10000 2513433025)

   ; Fishman18 test value from PARI: (62089911^10000)%(2^31-1) 
   (rng_test gsl_rng_fishman18 1 10000 330402013)

   ; Fishman2x test value from PARI: 
   ;   ((48271^10000)%(2^31-1) - (40692^10000)%(2^31-249))%(2^31-1) 
   (rng_test gsl_rng_fishman2x 1 10000 540133597)

   #| Knuthran2 test value from PARI: 
     { xn1=1; xn2=1; for (n=1,10000, 
            xn = (271828183*xn1 - 314159269*xn2)%(2^31-1);
            xn2=xn1; xn1=xn; print(xn); ) }  |#
   (rng_test gsl_rng_knuthran2 1 10000 1084477620)

   ; Knuthran test value taken from p188 in Knuth Vol 2. 3rd Ed 
   ;(rng_test gsl_rng_knuthran 310952 ($ "1009 * 2009 + 1") 461390032)
   (rng_test gsl_rng_knuthran 310952 (+ (* 1009 2009) 1) 461390032)

   ; Knuthran improved test value from Knuth's source 
   (rng_test gsl_rng_knuthran2002 310952 1 708622036)
   (rng_test gsl_rng_knuthran2002 310952 2 1005450560)
   ;(rng_test gsl_rng_knuthran2002 310952 ($ "100 * 2009 + 1") 995235265)
   (rng_test gsl_rng_knuthran2002 310952 (+ (* 100 2009) 1) 995235265)
   ;(rng_test gsl_rng_knuthran2002 310952 ($ "1009 * 2009 + 1") 704987132)
   (rng_test gsl_rng_knuthran2002 310952 (+ (* 1009 2009) 1) 704987132)

   ; Lecuyer21 test value from PARI: (40692^10000)%(2^31-249) 
   (rng_test gsl_rng_lecuyer21 1 10000 2006618587)

   ; Waterman14 test value from PARI: (1566083941^10000)%(2^32) 
   (rng_test gsl_rng_waterman14 1 10000 3776680385)

   ; specific tests of known results for 10000 iterations with seed = 6 

   #| Coveyou test value from PARI:
     x=6; for(n=1,10000,x=(x*(x+1))%(2^32);print(x);) |#

   (rng_test gsl_rng_coveyou 6 10000 1416754246)

   ; Fishman20 test value from PARI: (6*48271^10000)%(2^31-1) 
   (rng_test gsl_rng_fishman20 6 10000 248127575)

   (rng_test gsl_rng_ranlux 314159265 10000 12077992)
   (rng_test gsl_rng_ranlux389 314159265 10000 165942)

   (rng_test gsl_rng_ranlxs0 1 10000 11904320)
   ; 0.709552764892578125 * ldexp(1.0,24) 

   (rng_test gsl_rng_ranlxs1 1 10000 8734328)
   ; 0.520606517791748047 * ldexp(1.0,24) 

   (rng_test gsl_rng_ranlxs2 1 10000 6843140)
   ; 0.407882928848266602 * ldexp(1.0,24) 

   (rng_test gsl_rng_ranlxd1 1 10000 1998227290)
   ; 0.465248546261094020 * ldexp(1.0,32) 

   (rng_test gsl_rng_ranlxd2 1 10000 3949287736)
   ; 0.919515205581550532 * ldexp(1.0,32) 

   (rng_test gsl_rng_slatec 1 10000 45776)
   (rng_test gsl_rng_uni 1 10000 9214)
   (rng_test gsl_rng_uni32 1 10000 1155229825)
   (rng_test gsl_rng_zuf 1 10000 3970)

   (rng_test gsl_rng_r250 1 10000 1100653588)
   (rng_test gsl_rng_mt19937 4357 1000 1186927261)
   (rng_test gsl_rng_mt19937_1999 4357 1000 1030650439)
   (rng_test gsl_rng_mt19937_1998 4357 1000 1309179303)
   (rng_test gsl_rng_tt800 0 10000 2856609219)

   (rng_test gsl_rng_ran0 0 10000 1115320064)
   (rng_test gsl_rng_ran1 0 10000 1491066076)
   (rng_test gsl_rng_ran2 0 10000 1701364455)
   (rng_test gsl_rng_ran3 0 10000 186340785)

   (rng_test gsl_rng_ranmar 1 10000 14428370)

   (rng_test gsl_rng_rand48 0 10000 #xDE095043)
   (rng_test gsl_rng_rand48 1 10000 #xEDA54977)

   (rng_test gsl_rng_random_glibc2 0 10000 1908609430)
   (rng_test gsl_rng_random8_glibc2 0 10000 1910041713)
   (rng_test gsl_rng_random32_glibc2 0 10000 1587395585)
   (rng_test gsl_rng_random64_glibc2 0 10000 52848624)
   (rng_test gsl_rng_random128_glibc2 0 10000 1908609430)
   (rng_test gsl_rng_random256_glibc2 0 10000 179943260)

   (rng_test gsl_rng_random_bsd 0 10000 1457025928)
   (rng_test gsl_rng_random8_bsd 0 10000 1910041713)
   (rng_test gsl_rng_random32_bsd 0 10000 1663114331)
   (rng_test gsl_rng_random64_bsd 0 10000 864469165)
   (rng_test gsl_rng_random128_bsd 0 10000 1457025928)
   (rng_test gsl_rng_random256_bsd 0 10000 1216357476)

   (rng_test gsl_rng_random_libc5 0 10000 428084942)
   (rng_test gsl_rng_random8_libc5 0 10000 1910041713)
   (rng_test gsl_rng_random32_libc5 0 10000 1967452027)
   (rng_test gsl_rng_random64_libc5 0 10000 2106639801)
   (rng_test gsl_rng_random128_libc5 0 10000 428084942)
   (rng_test gsl_rng_random256_libc5 0 10000 116367984)

   (rng_test gsl_rng_ranf 0 10000 2152890433)
   (rng_test gsl_rng_ranf 2 10000 339327233)))

(run-tests rng-tests)


