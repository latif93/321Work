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
            (type-case Value*Store (interp init-expr ds st)  ;
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
  (define (struct-interp struct init-expr ds st)
       (interp (first (init-expr)) init-expr ds st)
      (struct-interp (rest init-expr) (rest init-expr) ds st))
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

     (define (rwfae-struct-contract s-expr)
  (andmap (lambda (s) (and (symbol? (first s)) (number? (second s))))
       s-expr))
     (define (rfae-struct-contract s-expr)
  (andmap (lambda (s) (and (r-id? (first s)) (r-num? (second s))))
       s-expr))
(define-type RFAE
  [r-num (n number?)]
  [r-add (lhs RFAE?)
       (rhs RFAE?)]
  [r-sub (lhs RFAE?)
       (rhs RFAE?)]
  [r-fun (param-name symbol?)
       (body RFAE?)]
  [r-app (fun-expr RFAE?)
       (arg-expr RFAE?)]
  [r-id (name symbol?)]
  [r-struct (rest-pairs rfae-struct-contract)]
  [r-get (init-expr RFAE?) (id RFAE?)]
  [r-set (struct-expr RFAE?)
          (id RFAE?)
          (val RFAE?)]
  [r-seqn (expr1 RFAE?)
        (expr2 RFAE?)])


(define-type RWFAE
  [rw-num (n number?)]
  [rw-add (lhs RWFAE?)
       (rhs RWFAE?)]
  [rw-sub (lhs RWFAE?)
       (rhs RWFAE?)]
  [rw-fun (param-name (listof symbol?))
       (body RWFAE?)]
  [rw-app (fun-expr RWFAE?)
       (arg-expr (listof RWFAE?))]
  [rw-id (name symbol?)]
  [rw-struct (rest-pairs rwfae-struct-contract)]
  [rw-get (init-expr RWFAE?) (id RWFAE?)]
  [rw-set (struct-expr RWFAE?)
          (id RWFAE?)
          (val RWFAE?)]
  [rw-seqn (expr1 RWFAE?)
        (expr2 RWFAE?)])

(define (parse-struct lst)
  (map (lambda (s) (list(r-id (first s)) (r-num (second s))))
       lst)
  )
(define (nparse s-exp)
  (cond [(number? s-exp)
         (rw-num s-exp)]
        [(symbol? s-exp)
         (rw-id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'nparse "the empty list is not a valid RWFAE"))
         (case (first s-exp)
           [(+)
            (check-pieces s-exp "add" 3)
            (rw-add (nparse (second s-exp))
                 (nparse (third s-exp)))]
           [(-)
            (check-pieces s-exp "sub" 3)
            (rw-sub (nparse (second s-exp))
                 (nparse (third s-exp)))]
           [(fun)
            (check-pieces s-exp "fun" 3)
         ;   (check-pieces (second s-exp) "parameter list" 1)
            (rw-fun (second s-exp)
                 (nparse (third s-exp)))]
           [(with) ; in lieu of a compiler
            (check-pieces s-exp "with" 3)
            (check-pieces (second s-exp) "with binding pair" 2)
            (unless (symbol? (first (second s-exp)))
              (error 'nparse "expected variable name, got ~a" (first (second s-exp))))
            (rw-app (fun (first (second s-exp)) (nparse (third s-exp)))
                 (nparse (second (second s-exp))))]
           [(get)
         ;   (check-pieces s-exp "newbox" 2)
            (rw-get (nparse(second s-exp))
                (nparse(third s-exp)))]
           [(struct)
        ;    (check-pieces s-exp "setbox" 3)
            (rw-struct (rest s-exp))]
           [(set)
         ;   (check-pieces s-exp "openbox" 2)
            (rw-set (nparse(second s-exp)) (nparse(third s-exp)) (nparse(fourth s-exp)))]
           [(seqn)
            (check-pieces s-exp "seqn" 3)
            (rw-seqn (nparse (second s-exp))
                  (nparse (third s-exp)))]
           [else
        ;    (check-pieces s-exp "app" 2)
            (rw-app (nparse (first s-exp))
                 (map nparse (rest s-exp)))])]
        [else
         (error 'nparse "expected RWFAE got ~a" s-exp)]))

 ;`{get {struct {x 0} {y 1}} x}
#|
(nparse `{struct {x 0} {y 1}})
(nparse `{struct {x 0} {y 1} {z 1} {a 1}})
(nparse `{get {struct {x 0} {y 1} {z 1} {a 1}}x})
(nparse `{set {struct {a 2}} a 4})
(nparse `{seqn {set b x 2} {f f}})
(nparse `0)
(nparse `x)
(nparse `{fun {x} x})
(nparse `{fun {x} {+ 1 x}})
(nparse `{fun {x} {- x 1}})
|#
(define (maybe-constant-fold folder op l r)
  (if (and (num? l) (num? r))
      (num (folder (num-n l) (num-n r)))
      (op l r)))


(define (compile an-fwae)
  (type-case RWFAE an-fwae
    [rw-num (n) (r-num n)]
    [rw-add (l r) (maybe-constant-fold
                  +
                  r-add
                  (compile l)
                  (compile r))]
    [rw-sub (l r) (maybe-constant-fold
                  -
                  r-sub
                  (compile l)
                  (compile r))]
    [rw-id (name) (r-id name)]
    [rw-fun (param-name body)
            (if(empty? param-name)
               (error 'compile "nullary function")
            (if (empty? (rest param-name))
                (r-fun (first param-name) (compile body))
           (r-fun (first param-name) (compile(rw-fun (rest param-name) body)))))]
    [rw-struct (rest-pairs) (r-struct (parse-struct rest-pairs))]
    [rw-get (structs id-to-get) (r-get (compile structs) (compile id-to-get))]
    [rw-set (struct address-to-change new-val) (r-set (compile struct) (compile address-to-change) (compile new-val))]
    [rw-seqn (expr1 expr2) (r-seqn (compile expr1) (compile expr2))]
    [rw-app (fun-expr arg-expr)
            (if (empty? arg-expr)
                (error 'compile "nullary application")
            (if (empty? (remove (last arg-expr) arg-expr))
           (r-app (compile fun-expr)
                (compile (first arg-expr)))
           (r-app  (compile (rw-app fun-expr (remove (last arg-expr) arg-expr)))(compile (last arg-expr))) ))]))
#|
(compile(nparse `0))
(compile(nparse `x))
(compile(nparse `{fun {x y} {+ x y}}))
(compile(nparse `{fun {x} {+ 1 x}}))
(compile(nparse `{fun {x} {- x 1}}))
(compile(nparse `{struct {x 0} {y 1}}))
(compile(nparse `{struct {x 0} {y 1} {z 1} {a 1}}))
(compile(nparse `{f x y z}))
(compile(nparse `{fun {x y} {+ x x}}))
(compile(nparse `{get {struct {x 0} {y 1} {z 1} {a 1}}x}))
(compile(nparse `{set {struct {a 2}} a 4}))
(compile(nparse `{seqn {set b x 2} {f f}}))
|#



;interp: struct is just newbox but sort of mapped over the list of them, sort of like a parser
;interp: get is just openbox
;interp: set is just setbox

;so basically, only struct is different because it causees multiple, rather than 1 binding to get stored. the other functions
;just look em up and/or modify within the store