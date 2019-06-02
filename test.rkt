#lang racket

(require "rng.rkt"
         rackunit
         rackunit/text-ui)


(define-check (rng_test T seed n result)
  (begin
    
    (define r (gsl_rng_init T seed))

    (define k    
      (foldl (lambda (i acc) (gsl_rng_get r)) 0 (range n)))
    
    (when (not (= k result))        
      (with-check-info (['rng (string-info (gsl_rng_name r))]
                        ['actual k]
                        ['expected result])
        (fail-check)))))
    

(define rng-tests
  (test-suite
   "gsl_rng_..."
   
   
   ; specific tests of known results for 10000 iterations with seed = 1 
   (rng_test 'rand 1 10000 1910041713)
   (rng_test 'randu 1 10000 1623524161)
   (rng_test 'cmrg 1 10000 719452880)
   (rng_test 'minstd 1 10000 1043618065)
   (rng_test 'mrg 1 10000 2064828650)
   (rng_test 'taus 1 10000 2733957125)
   (rng_test 'taus2 1 10000 2733957125)
   (rng_test 'taus113 1 1000 1925420673)
   (rng_test 'transputer 1 10000 1244127297)
   (rng_test 'vax 1 10000 3051034865)   

   ; Borosh13 test value from PARI: (1812433253^10000)%(2^32) 
   (rng_test 'borosh13 1 10000 2513433025)

   ; Fishman18 test value from PARI: (62089911^10000)%(2^31-1) 
   (rng_test 'fishman18 1 10000 330402013)

   ; Fishman2x test value from PARI: 
   ;   ((48271^10000)%(2^31-1) - (40692^10000)%(2^31-249))%(2^31-1) 
   (rng_test 'fishman2x 1 10000 540133597)

   #| Knuthran2 test value from PARI: 
     { xn1=1; xn2=1; for (n=1,10000, 
            xn = (271828183*xn1 - 314159269*xn2)%(2^31-1);
            xn2=xn1; xn1=xn; print(xn); ) }  |#
   (rng_test 'knuthran2 1 10000 1084477620)

   ; Knuthran test value taken from p188 in Knuth Vol 2. 3rd Ed 
   ;(rng_test gsl_rng_knuthran 310952 ($ "1009 * 2009 + 1") 461390032)
   (rng_test 'knuthran 310952 (+ (* 1009 2009) 1) 461390032)

   ; Knuthran improved test value from Knuth's source 
   (rng_test 'knuthran2002 310952 1 708622036)
   (rng_test 'knuthran2002 310952 2 1005450560)
   ;(rng_test gsl_rng_knuthran2002 310952 ($ "100 * 2009 + 1") 995235265)
   (rng_test 'knuthran2002 310952 (+ (* 100 2009) 1) 995235265)
   ;(rng_test gsl_rng_knuthran2002 310952 ($ "1009 * 2009 + 1") 704987132)
   (rng_test 'knuthran2002 310952 (+ (* 1009 2009) 1) 704987132)

   ; Lecuyer21 test value from PARI: (40692^10000)%(2^31-249) 
   (rng_test 'lecuyer21 1 10000 2006618587)

   ; Waterman14 test value from PARI: (1566083941^10000)%(2^32) 
   (rng_test 'waterman14 1 10000 3776680385)

   ; specific tests of known results for 10000 iterations with seed = 6 

   #| Coveyou test value from PARI:
     x=6; for(n=1,10000,x=(x*(x+1))%(2^32);print(x);) |#

   (rng_test 'coveyou 6 10000 1416754246)

   ; Fishman20 test value from PARI: (6*48271^10000)%(2^31-1) 
   (rng_test 'fishman20 6 10000 248127575)

   (rng_test 'ranlux 314159265 10000 12077992)
   (rng_test 'ranlux389 314159265 10000 165942)

   (rng_test 'ranlxs0 1 10000 11904320)
   ; 0.709552764892578125 * ldexp(1.0,24) 

   (rng_test 'ranlxs1 1 10000 8734328)
   ; 0.520606517791748047 * ldexp(1.0,24) 

   (rng_test 'ranlxs2 1 10000 6843140)
   ; 0.407882928848266602 * ldexp(1.0,24) 

   (rng_test 'ranlxd1 1 10000 1998227290)
   ; 0.465248546261094020 * ldexp(1.0,32) 

   (rng_test 'ranlxd2 1 10000 3949287736)
   ; 0.919515205581550532 * ldexp(1.0,32) 

   (rng_test 'slatec 1 10000 45776)
   (rng_test 'uni 1 10000 9214)
   (rng_test 'uni32 1 10000 1155229825)
   (rng_test 'zuf 1 10000 3970)

   (rng_test 'r250 1 10000 1100653588)
   (rng_test 'mt19937 4357 1000 1186927261)
   (rng_test 'mt19937_1999 4357 1000 1030650439)
   (rng_test 'mt19937_1998 4357 1000 1309179303)
   (rng_test 'tt800 0 10000 2856609219)

   (rng_test 'ran0 0 10000 1115320064)
   (rng_test 'ran1 0 10000 1491066076)
   (rng_test 'ran2 0 10000 1701364455)
   (rng_test 'ran3 0 10000 186340785)

   (rng_test 'ranmar 1 10000 14428370)

   (rng_test 'rand48 0 10000 #xDE095043)
   (rng_test 'rand48 1 10000 #xEDA54977)

   (rng_test 'random_glibc2 0 10000 1908609430)
   (rng_test 'random8_glibc2 0 10000 1910041713)
   (rng_test 'random32_glibc2 0 10000 1587395585)
   (rng_test 'random64_glibc2 0 10000 52848624)
   (rng_test 'random128_glibc2 0 10000 1908609430)
   (rng_test 'random256_glibc2 0 10000 179943260)

   (rng_test 'random_bsd 0 10000 1457025928)
   (rng_test 'random8_bsd 0 10000 1910041713)
   (rng_test 'random32_bsd 0 10000 1663114331)
   (rng_test 'random64_bsd 0 10000 864469165)
   (rng_test 'random128_bsd 0 10000 1457025928)
   (rng_test 'random256_bsd 0 10000 1216357476)

   (rng_test 'random_libc5 0 10000 428084942)
   (rng_test 'random8_libc5 0 10000 1910041713)
   (rng_test 'random32_libc5 0 10000 1967452027)
   (rng_test 'random64_libc5 0 10000 2106639801)
   (rng_test 'random128_libc5 0 10000 428084942)
   (rng_test 'random256_libc5 0 10000 116367984)

   (rng_test 'ranf 0 10000 2152890433)
   (rng_test 'ranf 2 10000 339327233)
   
   (check-equal? (gsl_rng_min (gsl_rng_init 'mt19937 0)) 0 "min mt19937")
   (check-equal? (gsl_rng_max (gsl_rng_init 'mt19937 0)) (- (expt 2 32) 1) "max mt19937")
   (check-true (flonum? (gsl_rng_uniform (gsl_rng_init 'mt19937 0))) "uniform double")
   (check-true (flonum? (gsl_rng_uniform_pos (gsl_rng_init 'mt19937 0))) "uniform double pos")
   (check-true (integer? (gsl_rng_uniform_int (gsl_rng_init 'mt19937 ) 1000)) "uniform integer")
   
   ; This test could fail with propability = 1/2^32 
   (check-not-equal? (gsl_rng_get (gsl_rng_init 'mt19937 )) (gsl_rng_get (gsl_rng_init 'mt19937 )) "different seed")))


(run-tests rng-tests)

;(for-each (lambda (i) (run-tests rng-tests)) (range 5))


