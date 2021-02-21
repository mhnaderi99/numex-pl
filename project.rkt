;; PL Project - Fall 2020
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool (bool)   #:transparent)

(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)
(struct mult  (e1 e2)  #:transparent)
(struct div  (e1 e2)  #:transparent)
(struct neg  (e1)  #:transparent)
(struct andalso  (e1 e2)  #:transparent)
(struct orelse  (e1 e2)  #:transparent)
(struct cnd  (e1 e2 e3)  #:transparent)
(struct iseq  (e1 e2)  #:transparent)
(struct ifnzero  (e1 e2 e3)  #:transparent)
(struct ifleq  (e1 e2 e3 e4)  #:transparent)


(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

(struct with  (s e1 e2)  #:transparent)
(struct apair  (e1 e2)  #:transparent)
(struct 1st  (e1)  #:transparent)
(struct 2nd  (e1)  #:transparent)

(struct with*  (lst e)  #:transparent)

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent)

(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

(struct ifneq (e1 e2 e3 e4) #:transparent)
(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions

;; Problem 1

(define (racketlist->numexlist xs) (cond [(null? xs) (munit)]
                                         [true (apair (car xs) (racketlist->numexlist (cdr xs)))]))


(define (numexlist->racketlist xs) (cond [(munit? xs) '()]
                                         [else (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))]))


;; Problem 2

;; lookup a variable in an environment
;; Complete this function

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [true (envlookup (cdr env) str)]))

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
        [(boolean? e) e]
        [(number? e) e]
        [(string? e) e]
        [(munit? e) e]
        [(var? e) 
         (envlookup env (var-string e))]
        [(bool? e)
         (let ([v (eval-under-env (bool-bool e) env)])
           (if (boolean? v) (bool v) (error "bool must be boolean"))
           )]
        [(num? e)
         (let ([v (eval-under-env (num-int e) env)])
           (if (integer? v) (num v) (error "num must be integer"))
           )]
        [(neg? e)
         (let ([v (eval-under-env (neg-e1 e) env)])
           (if (num? v)
           (num (* -1 (num-int v)))
             (if (bool? v)
                 (bool (not (bool-bool v)))
                 (error "neg only accepts integer and boolean"))))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]

        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient (num-int v1) 
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]
        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (if (bool? v1)
               (if (equal? v1 (bool #t)) (eval-under-env (orelse (andalso-e2 e) (bool #f)) env) (bool #f))
               (error "NUMEX andalso applied to non-boolean")))]
        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (if (bool? v1)
               (if (equal? (bool-bool v1) #f) (eval-under-env (andalso (orelse-e2 e) (bool #t)) env) (bool #t))
               (error "NUMEX orelse applied to non-boolean")))]
        [(cnd? e) 
         (let ([v (eval-under-env (cnd-e1 e) env)])
           (if (bool? v)
               (if (equal? v (bool #t)) (eval-under-env (cnd-e2 e) env) (eval-under-env (cnd-e3 e) env))
               (error "NUMEX cnd applied to non-boolean")))]
        [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (if (nor (nor (num? v1) (bool? v1)) (nor (num? v2) (bool? v2)))
               (if (equal? v1 v2) (bool #t) (bool #f))
               (error "NUMEX cnd applied to non-boolean and non-number")))]
        [(ismunit? e) 
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v)
               (bool #t)(bool #f)))]
        [(1st? e) 
         (let ([v (eval-under-env (1st-e1 e) env)])
           (if (apair? v)
               (apair-e1 v)(error "1st must apply to apair")))]
        [(2nd? e) 
         (let ([v (eval-under-env (2nd-e1 e) env)])
           (if (apair? v)
               (apair-e2 v)(error "2nd must apply to apair")))]
        [(ifnzero? e) 
         (let ([v (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v)
               (if (equal? v (num 0))
               (eval-under-env (ifnzero-e3 e) env)(eval-under-env (ifnzero-e2 e) env))
               (error "ifnzero must be num"))
           )]
        [(ifleq? e) 
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1) (num? v2))
           (if (> (num-int v1)(num-int v2)) (eval-under-env (ifleq-e4 e) env) (eval-under-env (ifleq-e3 e) env))
           (error "ifleq must be num"))
           )]
        [(apair? e) 
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2)
           )]
        [(key? e)
         (let ([v (eval-under-env (key-e e) env)])
           (key (key-s e) v)
           )
        ]
        [(record? e)
         (let ([k (eval-under-env (record-k e) env)]
               [r (eval-under-env (record-r e) env)])
           (if (and (key? k) (or (munit? r) (record? r)))
             (record k r)
            (error "record must apply to a key followed by a record or munit"))
           )
        ]
        [(value? e)
         (let ([s (eval-under-env (value-s e) env)]
               [r (eval-under-env (value-r e) env)])
           (if (and (string? s) (or (record? r) (munit? r)))
             (if (munit? r)
                 (munit) 
                          (let ([s2 (eval-under-env (key-s (record-k r)) env)])
                            (if (equal? s s2)
                                (eval-under-env (key-e (record-k r)) env)
                                (eval-under-env (value s (record-r r)) env)
                                )
                            )
                 )
            (error "error"))
           )
        ]
        [(with? e)
         (let ([v (eval-under-env (with-e1 e) env)])
           (eval-under-env (with-e2 e) (append env (list (cons (with-s e) v))))
          )
        ]
        [(with*? e)
         (if (null? (with*-lst e))
             (eval-under-env (with*-e e) env)
             (let ([k (car (car (with*-lst e)))]
                   [v (eval-under-env (cdr (car (with*-lst e))) env)]
                   [newe (with* (cdr (with*-lst e)) (with*-e e))])
               (eval-under-env newe (append env (list (cons k v))))
               )
           )
         
        ]
        
        [(ifneq? e)
         (let ([v1 (eval-under-env (ifneq-e1 e) env)]
               [v2 (eval-under-env (ifneq-e2 e) env)])
        
          (if (nor (nor (num? v1) (bool? v1)) (nor (num? v2) (bool? v2)))
              (if (not (equal? v1 v2)) (eval-under-env (ifneq-e3 e) env) (eval-under-env (ifneq-e4 e) env))
              (eval-under-env (ifneq-e4 e) env))
         )
        ]
        [(closure? e)
         e
        ]

        [(lam? e)
         (closure env e)]

        [(apply? e)
          (let ([fc (eval-under-env (apply-funexp e) env)])
            (if (closure? fc) (let ([fd (closure-f fc)]
                                    [ev (eval-under-env (apply-actual e) env)])                 
              (eval-under-env (lam-body fd) (cons (cons (lam-formal fd) ev) (cons (cons (lam-nameopt fd) fc) (closure-env fc)))))
              (error "must be closure")))]  
        
        ;[(letrec? e)
        ;
        ;   )
        ; ]
        
        ;; CHANGE add more cases here
        [else (error "undefined syntax")]
        ))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
  (let ([v1 (eval-exp e1)])
    (if (munit? v1) (eval-exp e2) (eval-exp e3))
    )
  )

(define (wiith* bs e2)
  
(eval-under-env e2 bs)
 
)


(define (iifneq e1 e2 e3 e4)
  (let ([v1 (eval-exp e1)]
               [v2 (eval-exp e2)])
    
           (if (nor (nor (num? v1) (bool? v1)) (nor (num? v2) (bool? v2)))
               (if (not (equal? v1 v2)) (eval-exp e3) (eval-exp e4))
               (eval-exp e4))
          )
  )

;; Problem 4

(define numex-filter
  (lambda (f)(
              (lambda (g lst)
                (
                 (if (munit? lst)
                     (munit)
                     (
                      (let ([v (eval-exp (f (car lst)))])
                        (if (not (num? v))
                            (error "not num")
                            (if (not (equal? v (num 0)))
                                (cons (car lst) (g (cdr lst)))
                                (g (cdr lst))
                              )
                           )
                        )
                      )
                    )
                 )
                )
              )
    )
  )

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
