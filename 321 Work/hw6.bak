#lang plai
(print-only-errors)

(define-type BFAE
  [num (n number?)]
  [add (lhs BFAE?)
       (rhs BFAE?)]
  [sub (lhs BFAE?)
       (rhs BFAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?)
       (body BFAE?)]
  [app (fun-expr BFAE?)
       (arg-expr BFAE?)]
  [newbox (init-expr BFAE?)]
  [setbox (box-expr BFAE?)
          (new-expr BFAE?)]
  [openbox (box-expr BFAE?)]
  [seqn (expr1 BFAE?)
        (expr2 BFAE?)])

(define-type Store
  [mtSto]
  [aSto (address integer?)
        (value BFAE-Value?)
        (rest Store?)])

(define-type Value*Store
  [v*s (v BFAE-Value?)
       (s Store?)])

(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body BFAE?)
            (ds DefSub?)]
  [boxV (address integer?)])

(define-type DefSub
  [mtSub]
  [aSub  (name symbol?)
         (value BFAE-Value?)
         (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> BFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid BFAE"))
         (case (first s-exp)
           [(+)
            (check-pieces s-exp "add" 3)
            (add (parse (second s-exp))
                 (parse (third s-exp)))]
           [(-)
            (check-pieces s-exp "sub" 3)
            (sub (parse (second s-exp))
                 (parse (third s-exp)))]
           [(fun)
            (check-pieces s-exp "fun" 3)
            (check-pieces (second s-exp) "parameter list" 1)
            (fun (first (second s-exp))
                 (parse (third s-exp)))]
           [(with) ; in lieu of a compiler
            (check-pieces s-exp "with" 3)
            (check-pieces (second s-exp) "with binding pair" 2)
            (unless (symbol? (first (second s-exp)))
              (error 'parse "expected variable name, got ~a" (first (second s-exp))))
            (app (fun (first (second s-exp)) (parse (third s-exp)))
                 (parse (second (second s-exp))))]
           [(newbox)
            (check-pieces s-exp "newbox" 2)
            (newbox (parse (second s-exp)))]
           [(setbox)
            (check-pieces s-exp "setbox" 3)
            (setbox (parse (second s-exp))
                    (parse (third s-exp)))]
           [(openbox)
            (check-pieces s-exp "openbox" 2)
            (openbox (parse (second s-exp)))]
           [(seqn)
            (check-pieces s-exp "seqn" 3)
            (seqn (parse (second s-exp))
                  (parse (third s-exp)))]
           [else
            (check-pieces s-exp "app" 2)
            (app (parse (first s-exp))
                 (parse (second s-exp)))])]
        [else
         (error 'parse "expected BFAE got ~a" s-exp)]))

(define (check-pieces s-exp expected n-pieces)
  (unless (and (list? s-exp)
               (= n-pieces (length s-exp)))
    (error 'parse "expected ~a got ~a" expected s-exp)))

;; ----------------------------------------------------------------------

;; interp-test : s-expression -> BFAE-Value?
(define (interp-test s-exp)
  (type-case Value*Store (interp (parse s-exp) (mtSub) (mtSto))
    [v*s (v s) v]))

;; interp : BFAE? DefSub? Store? -> Value*Store?
(define (interp a-bfae ds st) ; NEW
  (type-case BFAE a-bfae
    [num (n) (v*s (numV n) st)]
    [add (l r) (numop + l r ds st)]
    [sub (l r) (numop - l r ds st)]
    [id (name)
        (v*s (lookup name ds)
             st)]
    [fun (param-name body)
         (v*s (closureV param-name body ds)
              st)]
    [app (fun-expr arg-expr)
         (interp2 fun-expr arg-expr ds st
                  (lambda (fun-val arg-val st3)
                    (type-case BFAE-Value fun-val
                      [closureV (param-name body closed-ds)
                                (interp body
                                        (aSub param-name
                                              arg-val
                                              closed-ds)
                                        st3)]
                      [else (error 'interp "expected function, got ~a" fun-val)])))]
    [newbox (init-expr)
            (type-case Value*Store (interp init-expr ds st)
              [v*s (init-val st2)
                   (define address (malloc st2))
                   (v*s (boxV address)
                        (aSto address
                              init-val
                              st2))])]
    [openbox (box-expr)
             (type-case Value*Store (interp box-expr ds st)
               [v*s (box-val st2)
                    (type-case BFAE-Value box-val
                      [boxV (address)
                            (v*s (lookup-store address st2)
                                 st2)]
                      [else (error 'interp "expected box, got ~a" box-val)])])]
    [setbox (box-expr new-expr)
            (interp2 box-expr new-expr ds st
                     (lambda (box-val new-val st3)
                       (type-case BFAE-Value box-val
                         [boxV (address)
                               (v*s box-val
                                    (aSto address
                                          new-val
                                          st3))]
                         [else (error 'interp "expected box, got ~a" box-val)])))]
    [seqn (expr1 expr2)
          (interp2 expr1 expr2 ds st
                   (lambda (v1 v2 st3)
                     (v*s v2 st3)))]))

(define (interp2 e1 e2 ds st finish)
  (type-case Value*Store (interp e1 ds st)
    [v*s (v1 st2)
         (type-case Value*Store (interp e2 ds st2)
           [v*s (v2 st3)
                (finish v1 v2 st3)])]))

;; lookup-store : integer? Store? -> BFAE-Value?
(define (lookup-store address st)
  (type-case Store st
    [mtSto () (error 'interp "dangling pointer: ~a" address)]
    [aSto (a v rest)
          (if (= address a)
              v
              (lookup-store address rest))]))

(define (malloc st)
  (+ 1 (max-address st)))
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (a v rest)
          (max a (max-address rest))]))

;; numop : (number? number? -> number?) BFAE? BFAE? DefSub? Store? -> Value*Store?
(define (numop op l r ds st)
  (interp2 l r ds st
           (lambda (l-val r-val st3)
             (unless (numV? l-val)
               (error 'interp "expected number, got ~a" l-val))
             (unless (numV? r-val)
               (error 'interp "expected number, got ~a" r-val))
             (v*s (numV (op (numV-n l-val) (numV-n r-val)))
                  st3))))

;; lookup : symbol? DefSub? -> BFAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))

(test (interp-test `{openbox {setbox {newbox 3} 10}})
      (numV 10))

(test (interp-test `{with {b {newbox 2}}
                          {+ {seqn {setbox b 4}
                                   5}
                             {openbox b}}})
      (numV 9))
(test (interp-test `{with {b {newbox 2}}
                          {+ {openbox b}
                             {seqn {setbox b 4}
                                   5}}})
      (numV 7))

(test (interp-test `{with {b {newbox 10}}
                          {seqn
                           {setbox b 12}
                           {seqn
                            {setbox b 14}
                            {openbox b}}}})
      (numV 14))

(test (interp-test `{with {b1 {newbox 5}}
                          {with {b2 {newbox 7}}
                                {+ {openbox b1}
                                   {openbox b2}}}})
      (numV 12))

(test (interp-test `{with {b1 {newbox 8}}
                          {with {b2 {newbox 4}}
                                {seqn {setbox b1 6}
                                      {+ {openbox b1}
                                         {openbox b2}}}}})
      (numV 10))

(test (interp-test `{with {b1 {newbox 8}}
                          {with {b2 {newbox 4}}
                                {seqn {setbox b1 6}
                                      {seqn {setbox b2 8}
                                            {+ {openbox b1}
                                               {openbox b2}}}}}})
      (numV 14))

;; from last time
(test (interp-test `{newbox 10})
      (boxV 1))
(test (interp-test `{openbox {newbox 10}})
      (numV 10))
(test (interp-test `{openbox {newbox {newbox 5}}})
      (boxV 1))
(test (interp-test `{openbox {openbox {newbox {newbox 5}}}})
      (numV 5))
(test (interp-test `{seqn {openbox {newbox 10}}
                          {+ 2 1}})
      (numV 3))
(test (interp-test `{with {b {newbox 0}}
                          {seqn {setbox b 10}
                                {openbox b}}})
      (numV 10))

;; ----------------------------------------------------------------------

(test (interp-test `{fun {x} {+ x 1}})
      (closureV 'x (add (id 'x) (num 1))
                (mtSub)))
(test (interp-test `{with {y 3} {fun {x} {+ x y}}})
      (closureV 'x (add (id 'x) (id 'y))
                (aSub 'y (numV 3) (mtSub))))
(test (interp-test `{{with {y 3} {fun {x} {+ x y}}}
                     5})
      (numV 8))
(test (interp-test `{with {y 100}
                          {{with {y 3} {fun {x} {+ x y}}}
                           5}})
      (numV 8))

(test/exn (interp-test `{with {z {fun {x} {+ x y}}}
                              {with {y 10}
                                    {z 3}}})
          "free identifier")
;; A: 13 -- wrong
;; B: free identifier -- right

;; ----------

;; 5 -> 5
(test (interp-test `5)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp-test `{+ 1 2})
      (numV 3))
;; {- 3 4} -> -1
(test (interp-test `{- 3 4})
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp-test `{+ {+ 1 2} {- 3 4}})
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {+ x x}})
      (numV 6))
#|
x
|#
(test/exn (interp-test `z)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp-test `{+ {with {x {+ 1 2}}
                             {+ x x}}
                       {with {x {- 4 3}}
                             {+ x x}}})
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp-test `{+ {with {x {+ 1 2}}
                             {+ x x}}
                       {with {y {- 4 3}}
                             {+ y y}}})
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {with {x {- 4 3}}
                                {+ x x}}})
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {with {y {- 4 3}}
                                {+ x x}}})
      (numV 6))

;; ----------

(test (interp-test `{with {f {fun {x} {+ x 1}}}
                          {f 3}})
      (numV 4))
(test (interp-test `{{fun {x} {+ x 1}} 3})
      (numV 4))
(test (interp-test `{fun {x} {+ x 1}})
      (closureV 'x (parse `{+ x 1}) (mtSub)))
(test/exn (interp-test `{1 2})
          "expected function")
(test/exn (interp-test `{+ 1 {fun {x} x}})
          "expected number")
(test (interp-test `{with {f {with {x 3}
                                   {fun {y} {+ x y}}}}
                          {f 2}})
      (numV 5))
