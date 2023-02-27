#lang plai

#|
<AE> ::= <num>
      |  {+ <AE> <AE>}
      |  {- <AE> <AE>}
|#







































#|
<WAE> ::= <num>
       |  {+ <WAE> <WAE>}
       |  {- <WAE> <WAE>}
       |  {with {<id> <WAE>} <WAE>}
       |  <id>
|#




#|
{with {x {+ 1 2}}
      {+ x x}}
|#

#|
x
|#

#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}}
|#

#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}}
|#
#|
(test (interp (add (with 'x (add (num 1) (num 2))
                         (add (id 'x) (id 'x)))
                   (with 'y (sub (num 4) (num 3))
                         (add (id 'y) (id 'y)))))
      8)
|#

#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
#|
(test (interp (with 'x (add (num 1) (num 2))
                    (with 'x (sub (num 4) (num 3))
                          (add (id 'x) (id 'x)))))
      2)
|#

#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
#|
(test (interp (with 'x (add (num 1) (num 2))
                    (with 'y (sub (num 4) (num 3))
                          (add (id 'x) (id 'x)))))
      6)
|#

























#|
;; Keep program shape the same.
;; Replace free identifiers with the same name.
;; Leave binding and bound identifiers alone.
;; subst : WAE? symbol? number? -> WAE?
(define (subst expr name value)
  ...)
|#

#|
substitute 10 for x in {+ 1 x}
|#

#|
substitute 10 for x in y
|#

#|
substitute 10 for x in {- x 1}
|#
#|
(test (subst (sub (id 'x) (num 1))
             'x 10)
      (sub (num 10) (num 1)))
|#

#|
substitute 10 for x in {with {y 17} x}
|#
#|
(test (subst (with 'y (num 17) (id 'x))
             'x 10)
      (with 'y (num 17) (num 10)))
|#

#|
substitute 10 for x in {with {y x} y}
|#
#|
(test (subst (with 'y (id 'x) (id 'y))
             'x 10)
      (with 'y (num 10) (id 'y)))
|#

#|
substitute 10 for x in {with {x y} x}
i.e., {with {x 10}
            {with {x {+ x 1}}
                  x}}
|#
#|
(test (subst (with 'x (id 'y) (id 'x))
             'x 10)
      (with 'x (id 'y) (id 'x)))
|#
