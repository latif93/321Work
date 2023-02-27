#lang plai

#|
<AE> ::= <num>
       |  {+ <AE> <AE>}
       |  {- <AE> <AE>}
|#

(define-type AE
  [])