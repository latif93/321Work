#lang plai

;(print-only-errors)

(define-type FWAE ; ugh, name clashes
  [W-num  (n number?)]
  [W-add  (lhs FWAE?)
          (rhs FWAE?)]
  [W-sub  (lhs FWAE?)
          (rhs FWAE?)]
  [W-with (name symbol?)
          (named-expr FWAE?)
          (body FWAE?)]
  [W-id   (name symbol?)]
  [W-fun  (param-name symbol?)
          (body FWAE?)]
  [W-app  (fun-expr FWAE?)
          (arg-expr FWAE?)])

(define-type FAE
  [num  (n number?)]
  [add  (lhs FAE?)
        (rhs FAE?)]
  [sub  (lhs FAE?)
        (rhs FAE?)]
  [id   (name symbol?)]
  [fun  (param-name symbol?)
        (body FAE?)]
  [app  (fun-expr FAE?)
        (arg-expr FAE?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body FAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub (name  symbol?)
        (value FAE-Value?) ; NEW
        (rest  DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expr -> FWAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (W-num s-expr)]
        [(symbol? s-expr)
         (W-id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid FWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "+")
            (W-add (parse (second s-expr))
                   (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "-")
            (W-sub (parse (second s-expr))
                   (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (W-with (first (second s-expr))
                    (parse (second (second s-expr)))
                    (parse (third s-expr)))]
           [(fun)
            (check-pieces s-expr 3 "fun")
            (check-pieces (second s-expr) 1 "parameter list")
            (W-fun (first (second s-expr))
                   (parse (third s-expr)))]
           [else
            (check-pieces s-expr 2 "app")
            (W-app (parse (first s-expr)) (parse (second s-expr)))])]
        [else
         (error 'parse "expected FWAE, got ~a" s-expr)]))

(define (check-pieces s-expr n who)
  (unless (and (list? s-expr) (= (length s-expr) n))
    (error 'parse "expected ~a, got ~a" who s-expr)))

;; ----------------------------------------------------------------------------

;; compile : FWAE? -> FAE?
(define (compile an-fwae)
  (type-case FWAE an-fwae
    [W-num (n) (num n)]
    [W-add (l r) (maybe-constant-fold
                  +
                  add
                  (compile l)
                  (compile r))]
    [W-sub (l r) (maybe-constant-fold
                  -
                  sub
                  (compile l)
                  (compile r))]
    [W-with (name named-expr body)
            (app (fun name (compile body))
                 (compile named-expr))]
    [W-id (name) (id name)]
    [W-fun (param-name body)
           (fun param-name (compile body))]
    [W-app (fun-expr arg-expr)
           (app (compile fun-expr)
                (compile arg-expr))]))

(define (maybe-constant-fold folder op l r)
  (if (and (num? l) (num? r))
      (num (folder (num-n l) (num-n r)))
      (op l r)))

;; ----------------------------------------------------------------------------

;; interp : FAE? DefSub? -> FWAE-Value?
(define (interp an-fae ds) ; NEW
  (type-case FAE an-fae
    [num (n)   (numV n)]
    [add (l r) (numop + l r ds)]
    [sub (l r) (numop - l r ds)]
    [id (name)
        (lookup name ds)]
    [fun (param-name body)
         (closureV param-name
                   body
                   ds)]
    [app (fun-expr arg-expr)
         (define fun-val (interp fun-expr ds))
         (define arg-val (interp arg-expr ds))
         (unless (closureV? fun-val)
           (error 'interp "expected function, got ~a" fun-val))
         (interp (closureV-body fun-val)
                 (aSub (closureV-param-name fun-val)
                       arg-val
                       (closureV-ds fun-val)))]))

(define (numop op l r ds)
  (define l-val (interp l ds))
  (unless (numV? l-val)
    (error 'interp "expected number, got ~a" l-val))
  (define r-val (interp r ds))
  (unless (numV? r-val)
    (error 'interp "expected number, got ~a" r-val))
  (numV (op (numV-n l-val) (numV-n r-val))))

;; lookup : symbol? DefSub? -> FAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))

(define initial-def-sub (mtSub))

(test (interp (compile (parse `{fun {x} {+ x 1}}))
              initial-def-sub)
      (closureV 'x (compile (parse `{+ x 1}))
                (mtSub)))

(test (interp (compile
               (parse `{with {y 10}
                             {fun {x} {+ y x}}}))
              initial-def-sub)
      (closureV 'x
                (compile (parse `{+ y x}))
                (aSub 'y (numV 10) (mtSub))))

(test/exn (interp (compile (parse `{{fun {x} {+ y x}}
                                    {with {y 10} 5}}))
                  initial-def-sub)
          "free identifier")

(test (interp (compile (parse `{{with {y 10}
                                      {fun {x} {+ y x}}}
                                5}))
              initial-def-sub)
      (numV 15))

(test (interp (compile (parse `{with {x 10}
                                     {{fun {y} y}
                                      x}}))
              initial-def-sub)
      (numV 10))
(test (interp (compile (parse `{with {f {fun {y} y}}
                                     {with {x 10}
                                           {f x}}}))
              initial-def-sub)
      (numV 10))

(test/exn (interp
           (compile
            (parse `{with {z {fun {x} {+ x y}}}
                          {with {y 10}
                                {z y}}}))
           initial-def-sub)
          "free identifier")
;; A: 13 -- wrong
;; B: free identifier -- right

;; ----------------------------------------------------------------------

(test (compile (parse `{+ 1 2}))
      (num 3))
(test (compile (parse `{with {x 3} {+ x 2}}))
      (app (fun 'x (add (id 'x) (num 2)))
           (num 3)))
(test (compile (parse `{+ 3 {with {x 2} x}}))
      (add (num 3) (app (fun 'x (id 'x))
                        (num 2))))
(test (compile (parse `{with {x 2}
                             {with {y 3}
                                   {+ x y}}}))
      (app (fun 'x
                (app (fun 'y (add (id 'x) (id 'y)))
                     (num 3)))
           (num 2)))

;; ----------------------------------------------------------------------

;; 5 -> 5
(test (interp (compile (parse `5))
              initial-def-sub)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp (compile (parse `{+ 1 2}))
              initial-def-sub)
      (numV 3))
;; {- 3 4} -> -1
(test (interp (compile (parse `{- 3 4}))
              initial-def-sub)
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (compile (parse `{+ {+ 1 2} {- 3 4}}))
              initial-def-sub)
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {+ x x}}))
              initial-def-sub)
      (numV 6))
#|
x
|#
(test/exn (interp (compile (parse `x))
                  initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (compile (parse `{+ {with {x {+ 1 2}}
                                        {+ x x}}
                                  {with {x {- 4 3}}
                                        {+ x x}}}))
              initial-def-sub)
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (compile (parse `{+ {with {x {+ 1 2}}
                                        {+ x x}}
                                  {with {y {- 4 3}}
                                        {+ y y}}}))
              initial-def-sub)
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {with {x {- 4 3}}
                                           {+ x x}}}))
              initial-def-sub)
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {with {y {- 4 3}}
                                           {+ x x}}}))
              initial-def-sub)
      (numV 6))

;; ----------

(test (interp (compile (parse `{with {f {fun {x} {+ x 1}}}
                                     {f 3}}))
              initial-def-sub)
      (numV 4))
(test (interp (compile (parse `{{fun {x} {+ x 1}} 3}))
              initial-def-sub)
      (numV 4))
(test (interp (compile (parse `{fun {x} {+ x 1}}))
              initial-def-sub)
      (closureV 'x (compile (parse `{+ x 1})) (mtSub)))
(test/exn (interp (compile (parse `{1 2}))
                  initial-def-sub)
          "expected function")
(test/exn (interp (compile (parse `{+ 1 {fun {x} x}}))
                  initial-def-sub)
          "expected number")
(test (interp (compile (parse `{with {f {with {x 3}
                                              {fun {y} {+ x y}}}}
                                     {f 2}}))
              initial-def-sub)
      (numV 5))
