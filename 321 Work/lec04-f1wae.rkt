#lang plai

(print-only-errors)

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
        (bound-expr F1WAE?)
        (body-expr F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)
       (arg-expr F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (param-name symbol?)
          (body F1WAE?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> F1WAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (num s-expr)]
        [(symbol? s-expr)
         (id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid F1WAE"))
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
           [else
            (check-pieces s-expr 2 "app")
            (unless (symbol? (first s-expr))
              (error 'parse "expected function name, got ~a" (first s-expr)))
            (app (first s-expr)
                 (parse (second s-expr)))])]
        [else (error 'parse "expected F1WAE, got ~a" s-expr)]))

(define (check-pieces s-expr n-pieces who)
  (unless (= n-pieces (length s-expr))
    (error 'parse "expected ~a, got ~a" who s-expr)))

(test (parse `1) (num 1))
(test (parse `y) (id 'y))
(test (parse `{+ 1 2}) (add (num 1) (num 2)))
(test (parse `{- 1 2}) (sub (num 1) (num 2)))
(test (parse `{with {x 3} {+ x 2}}) (with 'x (num 3) (add (id 'x) (num 2))))
(test (parse `{f 10}) (app 'f (num 10)))
(test/exn (parse `{+ 1 2 3}) "expected add")

;; ----------------------------------------------------------------------

;; interp : F1WAE? (listof FunDef?) DefSub? -> number?
(define (interp an-f1wae fundefs ds)
  (type-case F1WAE an-f1wae
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
    [app (fun-name arg-expr)
         (define the-fundef (lookup-fundef fun-name fundefs))
         (type-case FunDef the-fundef
           [fundef (function-name param-name body)
                   (interp body
                           fundefs
                           (aSub param-name
                                 (interp arg-expr fundefs ds)
                                 (mtSub)))])]))

;; lookup : symbol? DefSub? -> number?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
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
              (list (fundef 'f 'x
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice 'y
                            (parse `{+ y y})))
              initial-def-sub)
      -20)
(test (interp (parse `{f 10})
              (list (fundef 'f 'x
                            (parse `{- 10 {twice {twice x}}}))
                    (fundef 'twice 'y
                            (parse `{+ y y})))
              initial-def-sub)
      -30)

;; from the slides
(test/exn (interp (parse `{with {y 2}
                                {f 10}})
                  (list (fundef 'f 'x
                                (parse `{+ y x})))
                  initial-def-sub)
          "free identifier")

(test (interp (parse `{with {y 2}
                            {f y}})
              (list (fundef 'f 'x
                            (parse `{+ x x})))
              initial-def-sub)
      4)