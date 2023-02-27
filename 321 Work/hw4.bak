#lang plai
(define eight-principles
  (list
   "Know your rights."
   "Acknowledge your sources."
   "Protect your work."
   "Avoid suspicion."
   "Do your own work."
   "Never falsify a record or permit another person to do so."
   "Never fabricate data, citations, or experimental results."
   "Always tell the truth when discussing your work with your instructor."))
;(print-only-errors)

(define-type FnWAE
  [num (n number?)]
  [add (lhs FnWAE?)
       (rhs FnWAE?)]
  [sub (lhs FnWAE?)
       (rhs FnWAE?)]
  [with (name symbol?)
        (bound-expr FnWAE?)
        (body-expr FnWAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)
       (arg-expr list?)]
  [if0 (test FnWAE?)
       (expr-1 FnWAE?)
       (expr-2 FnWAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (param-name list?)
          (body FnWAE?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> FnWAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (num s-expr)]
        [(symbol? s-expr)
         (id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid FnWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "add")
            (add (parse (second s-expr))
                 (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "sub")
            (sub (parse (second s-expr))
                 (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (with (first (second s-expr))
                  (parse (second (second s-expr)))
                  (parse (third s-expr)))]
           [(if0)
          ;  (check-pieces s-expr 3 "if0")
           (if0
            (parse(second s-expr))
            (parse(third s-expr))
            (parse(fourth s-expr)))
                ]
            
           [else
        ;    (check-pieces s-expr 2 "app")
            (unless (symbol? (first s-expr))
              (error 'parse "expected function name, got ~a" (first s-expr)))
            (app (first s-expr)
                ; (parse (second s-expr))
                  (map parse (rest s-expr))
                 )])]
        [else (error 'parse "expected FnWAE, got ~a" s-expr)]))


(define (parse-defn s-exp)
  (if (check-duplicates(rest (second s-exp)))
      (error 'parse-defn "bad syntax")
  (fundef (first(second s-exp))
          (rest (second s-exp))
          (parse (third s-exp)))))


(define (check-pieces s-expr n-pieces who)
  (unless (= n-pieces (length s-expr))
    (error 'parse "expected ~a, got ~a" who s-expr)))


(test (parse `1) (num 1))
(test (parse `y) (id 'y))
(test (parse `{+ 1 2}) (add (num 1) (num 2)))
(test (parse `{- 1 2}) (sub (num 1) (num 2)))
(test (parse `{with {x 3} {+ x 2}}) (with 'x (num 3) (add (id 'x) (num 2))))
(test (parse `{f 10}) (app 'f (list (num 10))))
(test/exn (parse `{+ 1 2 3}) "expected add")

;; ----------------------------------------------------------------------

;; interp : FnWAE? (listof FunDef?) DefSub? -> number?
(define (interp an-fnwae fundefs ds)
  (type-case FnWAE an-fnwae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [id (name) (lookup name ds)]
    [with (name named-expr body)
          (interp body
                  fundefs
                  (aSub name
                        (interp named-expr fundefs ds)
                        ds))]
    [if0 (test expr-1 expr-2)
           (if (equal? 0 (interp test fundefs ds))
                     (interp expr-1 fundefs ds)
                     (interp expr-2 fundefs ds))]
    [app (fun-name arg-expr)
         (define the-fundef (lookup-fundef fun-name fundefs))
         (type-case FunDef the-fundef
           [fundef (function-name param-name body)
                      (interp body
                           fundefs (try-to-subst-each-param an-fnwae fundefs ds param-name arg-expr body))
                   ])]))

(define (try-to-subst-each-param a-fnwae fundefs ds remaining-params arg-expr body)
 ; (print ds)
  (if (empty? remaining-params)
      ds
      (try-to-subst-each-param a-fnwae fundefs
                           (aSub  (first remaining-params)
                                 (interp (first arg-expr) fundefs ds)
                                 ds)
                           (rest remaining-params) (rest arg-expr) body) )
  )
;; lookup : symbol? DefSub? -> number?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub ()  (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))

;; lookup-fundef : symbol? (listof FunDef?) -> FunDef?
(define (lookup-fundef fun-name fundefs)
  (cond [(empty? fundefs)
         (error 'interp "undefined function: ~a" fun-name)]
        [(equal? (fundef-fun-name (first fundefs)) fun-name)
         (first fundefs)]
        [else
         (lookup-fundef fun-name (rest fundefs))]))


;; ----------------------------------------------------------------------
;; tests from last time, updated

(define initial-def-sub (mtSub))

(define (interp-expr an-fnwae fundefs)
  (interp an-fnwae fundefs initial-def-sub))

;; 5 -> 5
(test (interp (parse `5) '() initial-def-sub)
      5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '() initial-def-sub)
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '() initial-def-sub)
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '() initial-def-sub)
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '()
              initial-def-sub)
      6)
#|
x
|#
(test/exn (interp (parse `x) '() initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {x {- 4 3}}
                               {+ x x}}})
              '()
              initial-def-sub)
      8)
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {y {- 4 3}}
                               {+ y y}}})
              '()
              initial-def-sub)
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '()
              initial-def-sub)
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '()
              initial-def-sub)
      6)

(test/exn (interp (parse `{f 10})
                  (list)
                  initial-def-sub)
          "undefined function")
(test (interp (parse `{f 10})
              (list (fundef 'f '(x)
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice '(y)
                            (parse `{+ y y})))
              initial-def-sub)
      -20)
(test (interp (parse `{f 10})
              (list (fundef 'f '(x)
                            (parse `{- 10 {twice {twice x}}}))
                    (fundef 'twice '(y)
                            (parse `{+ y y})))
              initial-def-sub)
      -30)

;; from the slides
;(test/exn (interp (parse `{with {y 2}
;                                {f 10}})
;                  (list (fundef 'f '(x)
;                                (parse `{+ y x})))
;                  initial-def-sub)
;          "free identifier")

(test (interp (parse `{with {y 2}
                            {f y}})
              (list (fundef 'f '(x)
                            (parse `{+ x x})))
              initial-def-sub)
      4)


;(parse `{f 10})
(test(interp (parse `{f 20 10})
        (list (parse-defn '{deffun {f x y} {+ x y}}))
                  initial-def-sub)30)
          
(test(interp (parse '{if0 1 1 2})
              '()
              initial-def-sub)2)

(test (interp (parse '{if0 0 1 2}) '() initial-def-sub) 1)
(test (interp (parse '{if0 1 2 3}) '() initial-def-sub) 3)

(define mult-and-neg-deffuns
(list


`{deffun {mult x y} {if0 {neg? y} {- {mult x {+ y 1}} x}
                         {if0 y 0 {+ x {mult x {- y 1}}}}}}
`{deffun {neg? x} {if0 x 1 {new-helper x 0 x}}}
 `{deffun {new-helper x count y} {if0 {+ x {+ count count}}
                                     0
                                     {if0 {- count y}
                                          1
                                          {new-helper {- x 1} {+ 1 count} y}}}}
))

(test(interp-expr (app 'mult (list (num -10) (num -32)))
        (map parse-defn mult-and-neg-deffuns)) 320)

(test(interp-expr (app 'mult (list (num 10) (num -32)))
        (map parse-defn mult-and-neg-deffuns)) -320)

(test(interp-expr (app 'mult (list (num -10) (num 32)))
        (map parse-defn mult-and-neg-deffuns)) -320)

(test(interp-expr (app 'mult (list (num 0) (num -32)))
        (map parse-defn mult-and-neg-deffuns)) 0)

(test(interp-expr (app 'mult (list (num -10) (num 0)))
        (map parse-defn mult-and-neg-deffuns)) 0)

(test(interp-expr (app 'mult (list (num 0) (num 0)))
        (map parse-defn mult-and-neg-deffuns)) 0)

(test(interp-expr (app 'mult (list (num 1000) (num -1000)))
        (map parse-defn mult-and-neg-deffuns)) -1000000)

(test(interp-expr (app 'mult (list (num 40) (num 20)))
        (map parse-defn mult-and-neg-deffuns)) 800)
;free ids before arity
(test/exn (interp-expr (parse '{f x})
(list (parse-defn '{deffun {f a b c} c})))
"free identifier")

;bad syntax before arity
(test/exn (interp-expr (parse '{f 1 2 3})
(list (parse-defn '{deffun {f x x} {+ x x}})))
"bad syntax")

;bad syntax before undefined
(test/exn (interp-expr (parse '{f 1 2 3})
(list (parse-defn '{deffun {q x x} {+ x x}})))
"bad syntax")

;undefined before free ids
(test/exn (interp-expr (parse '{f x})
(list (parse-defn '{deffun {q a b c} c})))
"undefined function")

;undefined before arity
(test/exn (interp-expr (parse '{f 1 2})
(list (parse-defn '{deffun {q x} 1})))
"undefined function")

;bad syntax before free ids
(test/exn (interp-expr (parse '{f x})
(list (parse-defn '{deffun {q a a c} c})))
"bad syntax")
