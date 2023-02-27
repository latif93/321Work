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
#|
<WAE> ::= <num>
       |  {+ <WAE> <WAE>}
       |  {- <WAE> <WAE>}
       |  {with {<id> <WAE>} <WAE>}
       |  <id>
|#
(define-type WAE
  [num (n number?)]
  [add (l WAE?)
       (r WAE?)]
  [sub (l WAE?)
       (r WAE?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)]
  [id (name symbol?)])

(define (collect-binding-symbols wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (collect-binding-symbols l) (collect-binding-symbols r))]
    [sub (l r) (append (collect-binding-symbols l) (collect-binding-symbols r))]
    [with (name named-expr body) (append (list name) (collect-binding-symbols named-expr)
                                       (collect-binding-symbols body))]
    [id (name) '()]))    


 

 (define (binding-ids wae)
   (sort (remove-duplicates (collect-binding-symbols wae))symbol<?))
(test(binding-ids (with 'y (num 2) (with 'x (add (id 'x) (num 1)) (id 'y))))'(x y))
(test(binding-ids (num 2)) '())
(test(binding-ids (id 'y))'())
(test(binding-ids (add (num 3) (num 7)))'())
(test(binding-ids (with 'x (num 2) (id 'x)))'(x))
(test(binding-ids (with 'x (num 2) (with 'y (add (id 'x) (num 1)) (id 'z) )))'(x y))
(test(binding-ids (with 'x
      (num 5)
      (with 'x
            (num 3)
            (add (id 'x)
                 (id 'x))))) '(x))
(test(binding-ids (with 'x (num 2) (with 'x (add (id 'x) (id 'y)) (id 'x))))'(x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (collect-used-symbols wae)
   (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (collect-used-symbols l) (collect-used-symbols r))]
    [sub (l r) (append (collect-used-symbols l) (collect-used-symbols r))]
    [with (name named-expr body) (append  (collect-used-symbols named-expr)
                                      (collect-used-symbols body))]
    [id (name) (list name)]))

(define (collect-bound-symbols wae)
    (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (collect-bound-symbols l ) (collect-bound-symbols r ))]
    [sub (l r) (append (collect-bound-symbols l ) (collect-bound-symbols r ))]
    [with (name named-expr body)
          (if (member name (collect-used-symbols body))
              (append (list name) (collect-bound-symbols named-expr) (collect-bound-symbols body))
               (append (collect-bound-symbols named-expr) (collect-bound-symbols body)))
                 ]
    [id (name) '()]))
(define (is-used-symbol? symbol wae)
  (type-case WAE wae
    [num (n) false]
    [add (l r) (or (is-used-symbol? symbol l) (is-used-symbol? symbol r))]
    [sub (l r) (or (is-used-symbol? symbol l) (is-used-symbol? symbol r))]
    [with (name named-expr body) (or (is-used-symbol? symbol named-expr)
                                     (is-used-symbol? symbol body))]
    [id (name) (if (equal? name symbol) true false)]))

(define (bound-ids wae )
   (sort(remove-duplicates(type-case WAE wae
    [num (n) '()]
    [add (l r) (append (bound-ids l ) (bound-ids r ))]
    [sub (l r) (append (bound-ids l ) (bound-ids r ))]
    [with (name named-expr body)
          (if (member name (collect-used-symbols body))
              (append (list name) (bound-ids named-expr) (bound-ids body))
               (append (bound-ids named-expr) (bound-ids body)))
                 ]
    [id (name) '()]))symbol<?))


(test(bound-ids (with 'x  (num 2) (with 'y (add (id 'y) (num 1)) (id 'x))) )'(x))
(test(bound-ids (with 'x (num 2) (with 'y (add (id 'z) (num 1)) (id 'q))))'())
(test(bound-ids (with 'x(with 'y
            (num 3)
            (add (id 'x)
                 (id 'y)))
      (num 5))) '(y))
(test(bound-ids (with 'x (num 2) (with 'x (add (id 'x) (id 'y)) (id 'x))))'(x))
(test(bound-ids (with 'x (with 'y (id 'y) (add (id 'x) (id 'y))) (num 5)))'(y))
(test(bound-ids(with 'x (with 'y (num 4) (add (id 'x) (num 4))) (add (id 'y) (num 1))))'())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (free-ids wae)
 (sort (remove-duplicates (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (free-ids l) (free-ids r))]
    [sub (l r) (append (free-ids l) (free-ids r))]
    [with (name named-expr body) (if (is-used-symbol? name body)
                                     (append (remove name (free-ids body))
                                             (free-ids named-expr))
                                     (append (free-ids body)
                                             (free-ids named-expr)))]
    [id (name) (list name)])) symbol<?))

(test(free-ids (with 'x (num 2) (with 'y (add (id 'x) (num 1)) (id 'z))))'(z))

(test(free-ids (with 'x (num 2) (with 'y (add (id 'x) (num 1)) (id 'y))))'())
(test(free-ids (with 'x (num 2) (with 'y (add (id 'z) (num 1)) (id 'q))))'(q z))
(test(free-ids (with 'x (num 2) (with 'x (add (id 'x) (id 'y)) (id 'x))))'(y))


(test(free-ids (with 'x(with 'y
            (id 'y)
            (add (id 'x)

(id 'y)))
      (num 5))) '(x y))
(test(free-ids(with 'x
(with 'y (num 4) (add (id 'x) (num 4)))
(add (id 'y) (num 1))))'(x y))

(test(bound-ids (with 'x(with 'y
            (id 'y)
            (add (id 'x)
                 (num 1)))
      (num 5))) '())


(test(free-ids (with 'x(with 'y
            (num 3)
            (add (id 'x)
                 (id 'y)))
      (with 'q
            (num 3)
            (add (id 'z)
                 (id 'a))))) '(a x z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define (collect-shadowed-ids wae)
;    (filter (lambda (e) (not (number? e))) (flatten (collect-binding-symbols wae))))

;(define (build-shadowed-ids bind-lst shad-lst)
;  (if (empty? bind-lst)
;      shad-lst
;  (if(member (first bind-lst) shad-lst)
;     (build-shadowed-ids (rest bind-lst) (remove (first bind-lst) shad-lst))
;     (build-shadowed-ids (rest bind-lst) shad-lst))))

; (define(shadowed-ids wae)
;   (sort(build-shadowed-ids (binding-ids wae)(collect-shadowed-ids wae))symbol<?))


(define (shadowed-ids wae)
   (sort(remove-duplicates(type-case WAE wae
    [num (n) '()]
    [add (l r) (append (shadowed-ids l) (shadowed-ids r))]
    [sub (l r) (append (shadowed-ids l) (shadowed-ids r))]
    [with (name named-expr body) (if (member name (binding-ids body))
                                     (append (list name)
                                             (shadowed-ids named-expr)
                                             (shadowed-ids body))
                                     (append (shadowed-ids named-expr)
                                             (shadowed-ids body)))]
    [id (name) '()]))symbol<?))


            

(test(binding-ids (with 'x (num 2) (with 'x (add (id 'x) (num 1)) (id 'x))))'(x))
(test(shadowed-ids (with 'x (num 2) (with 'x (add (id 'x) (num 1)) (id 'x))))'(x))
(test(binding-ids (num 2))'())
(test(shadowed-ids (num 2)) '())
(test(binding-ids (id 'y))'())
(test(shadowed-ids (id 'y))'())
(test(binding-ids (add (num 3) (num 7)))'())
(test(shadowed-ids (add (num 3) (num 7)))'())
(test(binding-ids (with 'x (num 2) (id 'x)))'(x))
(test(shadowed-ids (with 'x (num 2) (id 'x)))'())
(test(binding-ids (with 'x (with 'y (add (id 'x) (num 1)) (id 'z) ) (with 'y (add (id 'x) (num 1)) (id 'z) )))'(x y))
(test(shadowed-ids (with 'x (with 'y (add (id 'x) (num 1)) (id 'z) ) (with 'y (add (id 'x) (num 1)) (id 'z) )))'())
(test(shadowed-ids (with 'x
      (num 5)
      (with 'x
            (num 3)
            (add (id 'x)
                 (id 'x))))) '(x))
(test(shadowed-ids (with 'x (num 2) (with 'x (add (id 'x) (id 'y)) (id 'x))))'(x))
(test (bound-ids(with 'x (num 3) (num 2)))'())
(test (free-ids(with 'x (num 3) (num 2)))'())


(test(shadowed-ids
 (with 'x (with 'y (add (id 'x) (num 1)) (id 'z)) (with 'y (add (id 'x) (num 1)) (id 'z))))'())
(test(shadowed-ids
 (with 'x (num 2) (with 'x (add (id 'x) (num 1)) (id 'x))))'(x))
(test(shadowed-ids
 (with 'x (num 2) (with 'y (with 'z (num 2) (with 'z (num 3) (id 'z))) (id 'x))))'(z))



(test(free-ids (with 'x (num 2) (with 'y (add (id 'x) (num 1)) (id 'z))))'(z))
(test(free-ids (with 'x(with 'y
            (id 'y)
            (add (id 'x)
                 (id 'y)))
      (num 5)))'(x y))
(test(free-ids (with 'x
(with 'y (num 4) (add (id 'x) (num 4))) 
(add (id 'y) (num 1))))'(x y))

(test(bound-ids (with 'x (with 'y (num 4) (add (id 'x) (num 4))) (add (id 'y) (num 1))))'()) ;() y x
(test(bound-ids (with 'x(with 'y
            (id 'y)
            (add (id 'x)
                 (id 'y)))
      (num 5)))'(y))
(test(bound-ids (with 'x (num 2) (with 'y (add (id 'x) (num 1)) (id 'z))))'(x))

(test(free-ids (with 'x (id 'x) (id 'x)))'(x))

(test(shadowed-ids
  (add
   (with 'x (num 1) (with 'x (id 'x) (num 1)))
   (with 'x (id 'x) (with 'x (id 'x) (id 'x)))))'(x))

(test(free-ids (add
                (with 'a (num 0) (num 0))
                (id 'a)))'(a))
(test(free-ids(with 'x
                    (with 'y (num 4) (add (id 'x) (num 4)))
                    (add (id 'y) (num 1))))'(x y))

(test(free-ids (with 'x (num 1) (with 'y (id 'x) (add (id 'x) (id 'y)))))'())

(test (free-ids (with 'x (num 1) (add (id 'x) (add (id 'x) (add (id 'x) (id 'x))))))'())
(test (collect-used-symbols (with 'a (num 0) (add (id 'a) (id 'a))))'(a a))
(test (free-ids (with 'a (num 0) (add (id 'a) (id 'a))))'())
