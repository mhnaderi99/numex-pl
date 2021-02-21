#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)


(define tests
  (test-suite
   "Project Tests"

   ; arithmetic functions test
     

(test-equal? "test59"
               (num 43)
               (eval-exp (apply (lam "incr" "x" (plus (var "x") (num 1))) (num 42))))


(test-equal? "test60"
               (bool #t)
               (eval-exp (apply (lam "neg" "x" (neg (var "x"))) (bool #f))))


(test-equal? "test61"
               (num 10)
               (eval-exp (apply (lam "mul" "x" (neg (mult (var "x") (num 2)))) (num -5))))



   
   (check-equal? (eval-exp (apply (lam "a" "b" (ifleq (var "b") (num 5) (plus (var "b") (num 3))
                                                      (apply (var "a") (mult (num 2) (num 3)))))
                                  (num 2))) (num 5) "test70")
   
    
     
    (check-equal? (eval-exp (closure '() (lam null "x" (var "x"))))
                 (closure '() (lam null "x" (var "x"))) "test90")


    (check-equal? (eval-exp (lam null "x" (var "x")))
                 (closure '() (lam null "x" (var "x"))) "test94")
   (check-equal? (eval-exp (with "x" (num 1) (lam null "a" (var "x"))))
                 (closure (list (cons "x" (num 1))) (lam null "a" (var "x"))) "test95")



   (check-exn exn:fail? (lambda () (eval-exp (apply (num 1) (num 2)))) "test116")




(check-exn exn:fail? (lambda () (eval-exp
                  (apply (apply numex-all-gt (bool #t) )
                   (apair (num 2) (apair (num 2) (apair (num -2) (munit)))))))
                  "test130")

(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-odd") (num 11)))) (bool #t) "test138")
(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-odd") (num 10)))) (bool #f) "test139")
(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-even") (num 11)))) (bool #f) "test140")
(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-even") (num 10)))) (bool #t) "test141")

   ))


(require rackunit/text-ui)
(require rackunit/log)
;; runs the test
;(run-tests tests)


(define result (run-tests tests))

(define out (open-output-file "grade.txt" #:exists 'replace))
(pretty-write (- 100 result) out #:newline? #t)
(pretty-write (test-log) out #:newline? #f)
(close-output-port out)

;(define out2 (open-output-file "summary.txt" #:exists 'replace))
;(write (test-log) out2)
;(close-output-port out2)