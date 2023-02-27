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
(print-only-errors)

(define-type FWAE
[W-num (n number?)]
[W-add (lhs FWAE?)
(rhs FWAE?)]
[W-sub (lhs FWAE?)
(rhs FWAE?)]
[W-with (name symbol?)
(named-expr FWAE?)
(body FWAE?)]
[W-id (name symbol?)]
[W-if0 (tst FWAE?)
(thn FWAE?)
(els FWAE?)]
[W-fun (params (listof symbol?))
(body FWAE?)]
[W-app (fun-expr FWAE?)
(arg-exprs (listof FWAE?))])

(define-type FAE
[num (n number?)]
[add (lhs FAE?) (rhs FAE?)]
[sub (lhs FAE?) (rhs FAE?)]
[id (name symbol?)]
[if0 (test FAE?) (then FAE?) (else FAE?)]
[fun (param symbol?) (body FAE?)]
[app (fun FAE?) (arg FAE?)])

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
              (error parse "expected variable name, got ~a" (first (second s-expr))))
            (W-with (first (second s-expr))
                    (parse (second (second s-expr)))
                    (parse (third s-expr)))]
           [(if0)
          ;  (check-pieces s-expr 3 "if0")
           (W-if0
            (parse(second s-expr))
            (parse(third s-expr))
            (parse(fourth s-expr)))
                ]
           [(fun)
            (check-pieces s-expr 3 "fun")
            (W-fun (second s-expr)
                   (parse (third s-expr)))]
           [else
        ;    (check-pieces s-expr 2 "app")
            (W-app (parse (first s-expr)) (map parse (rest s-expr)))])]
        [else
         (error 'parse "expected FWAE, got ~a" s-expr)]))

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
            (if(empty? param-name)
               (error 'compile "nullary function")
            (if (empty? (rest param-name))
                (fun (first param-name) (compile body))
           (fun (first param-name) (compile(W-fun (rest param-name) body)))))]
    [W-if0 (tst thn els)
            (if0 (compile tst) (compile thn) (compile els))]
    [W-app (fun-expr arg-expr)
            (if (empty? arg-expr)
                (error 'compile "nullary application")
            (if (empty? (remove (last arg-expr) arg-expr))
           (app (compile fun-expr)
                (compile (first arg-expr)))
           (app  (compile (W-app fun-expr (remove (last arg-expr) arg-expr)))(compile (last arg-expr))) ))]))
;; ----------------------------------------------------------------------
(define (check-pieces s-expr n who)
  (unless (and (list? s-expr) (= (length s-expr) n))
    (error 'parse "expected ~a, got ~a" who s-expr)))

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
    [if0 (tst thn els)
         (define condition (interp tst ds)) ;check if numv, and then if it is 0
           (if (equal? (numV 0) condition)
                     (interp thn ds)
                     (interp els ds))]
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
  (define r-val (interp r ds))
  (unless (numV? l-val)
    (error 'interp "expected number, got ~a" l-val))
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

(define (interp-expr an-fae)
  (interp an-fae initial-def-sub))

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



(test (compile(parse `{f x y z})) (app (app (app (id 'f) (id 'x)) (id 'y)) (id 'z)))
(test/exn(compile(parse `{{{f } y}z}))"nullary application")
(test/exn(compile(parse `{fun {} {-{+ a b}x}}))"nullary function")
(test(compile(parse `{fun {a b x} {-{+ a b}x}}))
     (fun 'a (fun 'b (fun 'x (sub (add (id 'a) (id 'b)) (id 'x))))))

(define factorial `{fun {n}
  {with {mk-rec
                  {fun {almost-fac}
                       {with {not-fac
                              {fun {not-fac}
                              {with {fac {fun {n} {{not-fac not-fac} n}}}
                                        {almost-fac fac}}}}
                             {not-fac not-fac}}}}
           {with {fac
                  {mk-rec {fun {fac}
                   {fun {n}
                        {if0 n
                             1
                             {with {mult
                                   {mk-rec{fun {mult} {fun {x y}
                                         {if0 y 0 {+ x {mult x {- y 1}}}}}}}} 
                             {mult n {fac {- n 1}}}}}}}}}
                 {fac n}}
           }})


(test(interp-expr(compile(parse `{,factorial 5})))(numV 120))
(test(interp-expr(compile(parse `{,factorial 4})))(numV 24))
(test(interp-expr(compile(parse `{,factorial 3})))(numV 6))
(test(interp-expr(compile(parse `{,factorial 2})))(numV 2))
(test(interp-expr(compile(parse `{,factorial 1})))(numV 1))
(test/exn (interp-expr (compile (parse `{1 2})))
          "expected function")

(test/exn (interp-expr (compile (parse `x)))
          "free identifier")

(test/exn (interp-expr (compile (parse `{+ 1 {fun {x} x}})))
          "expected number")

(test/exn (interp-expr (compile (parse `{+ {fun {x} x}
{1 2}})))
"expected function")
(define prime? `{fun {q}
  {with {mk-rec
                  {fun {almost-fac}
                       {with {not-fac
                              {fun {not-fac}
                              {with {fac {fun {n} {{not-fac not-fac} n}}}
                                        {almost-fac fac}}}}
                             {not-fac not-fac}}}}
                                               {with {fac
                  {mk-rec {fun {fac}
                   {fun {n}
                        {if0 n
                             0
                             {if0 {- n 1}
                                  0
                                  {if0 {- n 2}
                                       0
                                      {if0
      {with {evenly-divisible?
          {mk-rec{fun {evenly-divisible?} {fun {x y}
                                  {if0 x 0
                                      
                                         {if0
{with {neg?
          {mk-rec{fun {neg?} {fun {x}
                                  {if0 x 1
{with {new-helper
          {mk-rec{fun {new-helper} {fun {x y}
                                        {if0 x
                                     1 
                                     {if0 y
                                          0
                                          {new-helper {- x 1} {+ 1 y}}}}}}}}
                                       {new-helper x x}}}}}}}

                                          {neg? x}} 1 {evenly-divisible? {- x y} y}}}}}}}
                             {evenly-divisible? q {- n 1}}} 1 {fac {- n 1}}}}}}}}}}
                                                     {fac q}}}})

(test(interp-expr(compile(parse `(,prime? 1)))) (numV 0))
(test(interp-expr(compile(parse `{,prime? 2}))) (numV 0))
(test(interp-expr(compile(parse `{,prime? 3}))) (numV 0))
(test(interp-expr(compile(parse `{,prime? 4}))) (numV 1))
(test(interp-expr(compile(parse `{,prime? 5}))) (numV 0))
(test(interp-expr(compile(parse `{,prime? 6}))) (numV 1))
(test(interp-expr(compile(parse `{,prime? 7}))) (numV 0))
(test(interp-expr(compile(parse `{,prime? 8}))) (numV 1))
(test(interp-expr(compile(parse `{,prime? 9}))) (numV 1))
(test(interp-expr(compile(parse `{,prime? 10}))) (numV 1))
(test(interp-expr(compile(parse `{,prime? 23}))) (numV 0))
      (test/exn (interp-expr (compile (parse `{+ {fun {x} x}
{1 2}})))
"expected function")

    (test/exn (interp-expr (compile (parse `{+ {fun {x} y}
{1 2}})))
"expected function")

    (test/exn (interp-expr (compile (parse `{+ y
 {fun {x} x}})))
"free identifier")


