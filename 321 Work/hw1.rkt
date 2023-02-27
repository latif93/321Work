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

(define-type Tree
[positive-leaf (val natural?)]
[negative-leaf (val natural?)]
[interior-node (left Tree?) (right Tree?)])

;                                                       HW1 Code

(define (contains? tree num)
  (type-case Tree tree
    [positive-leaf (val) (= num val)]
    [negative-leaf (val) (= num (* -1 val))]
    [interior-node (left right) (or (contains? left num) (contains? right num))]))

(test (contains?  (interior-node (interior-node (positive-leaf 5)
(negative-leaf 2))
(positive-leaf 9)) 9)true)

(test (contains? 
(negative-leaf 2) -2)true)

(test (contains? 
(interior-node (interior-node (positive-leaf 5)
(negative-leaf 2))
(positive-leaf 9)) -3) false)

(test (contains? 
(positive-leaf 2) 2)true)

(test (contains? 
(positive-leaf 2) -2)false)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (smallest tree)
  (type-case Tree tree
    [positive-leaf (val) val]
    [negative-leaf (val) (* -1 val)]
    [interior-node (left right) (min (smallest left) (smallest right))]))

(test(smallest  (interior-node (interior-node (negative-leaf 15)
(positive-leaf 2))
(negative-leaf 9)))-15)

(test (smallest   (negative-leaf 15)) -15)
(test (smallest   (negative-leaf 0)) 0)
(test (smallest   (positive-leaf 0)) 0)

(test(smallest  (interior-node 
(negative-leaf 9)(negative-leaf 10)))-10)

(test(smallest  (interior-node 
(negative-leaf 10)(negative-leaf 10)))-10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-of-leaves tree)
  (type-case Tree tree
    [positive-leaf (val) val]
    [negative-leaf (val) (* -1 val)]
    [interior-node (left right) (+ (sum-of-leaves left) (sum-of-leaves right))]))

(define (balanced? tree)
  (= 0 (sum-of-leaves tree)))

(test(balanced?  (interior-node (interior-node (negative-leaf 15)
(positive-leaf 24))
(negative-leaf 9)))#t)

(test(balanced?  (positive-leaf 1))#f)
(test(balanced?  (positive-leaf 0))#t)
(test(balanced?  (negative-leaf 1))#f)
(test(balanced?  (negative-leaf 0))#t)

(test(balanced?  (interior-node (interior-node (negative-leaf 15)
(positive-leaf 24))
(interior-node (negative-leaf 32)
(positive-leaf 23))))#t)

(test(balanced?  (interior-node (interior-node (negative-leaf 15)
(positive-leaf 24))
(interior-node (negative-leaf 32)
(positive-leaf 22))))#f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (deep-balanced? tree)
    (let([deep? #t])(type-case Tree tree
    [positive-leaf (val) #t]
    [negative-leaf (val) #t]
    [interior-node (left right) (when (or(not(= 0 (sum-of-leaves left) )) (not(= 0 (sum-of-leaves right))))
                                  (set! deep? #f))
                   (when (and (balanced? tree) (deep-balanced? left) (deep-balanced? left) )
                     (set! deep? #t))
                   ])deep?))

(test(deep-balanced? (interior-node(interior-node (negative-leaf 15)
(positive-leaf 15))  (negative-leaf 0)))#t)

(test(deep-balanced? (interior-node (negative-leaf 15)
(positive-leaf 78)))#f)

(test(deep-balanced? (interior-node(interior-node (negative-leaf 15)
(positive-leaf 15))  (interior-node (negative-leaf 16)
(positive-leaf 16))))#t)

(test(deep-balanced? (positive-leaf 9))#t)
(test(balanced? (positive-leaf 1))#f)

(test(deep-balanced? (negative-leaf 9))#t)
(test(balanced? (negative-leaf 1))#f)

(test(deep-balanced? (interior-node (interior-node (negative-leaf 1) (positive-leaf 0))
                                    (interior-node (positive-leaf 1) (positive-leaf 0))))#f)
(test(deep-balanced? (interior-node (interior-node (negative-leaf 1) (positive-leaf 0))
                                    (interior-node (positive-leaf 0) (positive-leaf 0))))#f)
(test(deep-balanced? (interior-node (interior-node (interior-node (negative-leaf 0) (positive-leaf 0))
                                                   (interior-node (negative-leaf 1) (positive-leaf 0)))
                                    (interior-node (positive-leaf 0) (positive-leaf 0))))#f)
(test(deep-balanced? (interior-node (interior-node (interior-node (negative-leaf 0) (positive-leaf 0))
                                                   (interior-node (negative-leaf 1) (positive-leaf 1)))
                                    (interior-node (positive-leaf 0) (positive-leaf 0))))#t)
(test (deep-balanced? (interior-node (interior-node (negative-leaf 1) (positive-leaf 0)) (interior-node (positive-leaf 1) (positive-leaf 0))))#f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (negate tree)
  (type-case Tree tree
    [positive-leaf (val) (negative-leaf val)]
    [negative-leaf (val) (positive-leaf val)]
    [interior-node (left right) (interior-node (negate left) (negate right))]))

(test(negate (positive-leaf 9))(negative-leaf 9))
(test(negate (negative-leaf 1))(positive-leaf 1))

(test(sum-of-leaves (negate (interior-node(interior-node (negative-leaf 16)
(positive-leaf 15))  (negative-leaf 0))))1)

(test(sum-of-leaves (interior-node(interior-node (positive-leaf 16)
(negative-leaf 15))  (negative-leaf 0)))1)

(test(sum-of-leaves (negate(interior-node(interior-node (positive-leaf 16)
(negative-leaf 15))  (positive-leaf 0))))-1)

(test(sum-of-leaves (negate (interior-node(interior-node (negative-leaf 16)
(positive-leaf 15)) (interior-node (negative-leaf 16)
(positive-leaf 15)))))2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add tree num)
  (type-case Tree tree
    [positive-leaf (val) (if (> (+ val num) 0) (positive-leaf (+ val num)) (negative-leaf (* -1 (+ val num))))]
    [negative-leaf (val) (if (> (- val num) 0) (negative-leaf (- val num)) (positive-leaf (* -1 (- val num))))]
    [interior-node (left right) (interior-node (add left num) (add right num))]))

(test(add (positive-leaf 4) -4)(negative-leaf 0))
(test(add (negative-leaf 4) 4) (positive-leaf 0))

(test(add (interior-node(interior-node (negative-leaf 16)
(positive-leaf 15))  (negative-leaf 3)) 6)
     (interior-node(interior-node (negative-leaf 10)
(positive-leaf 21))  (positive-leaf 3)))

(test(add (interior-node(interior-node (negative-leaf 16)
(positive-leaf 15))  (negative-leaf 3)) -25)
     (interior-node(interior-node (negative-leaf 41)
(negative-leaf 10))  (negative-leaf 28)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (positive-thinking tree)
  (type-case Tree tree
    [positive-leaf (val) (positive-leaf val)]
    [negative-leaf (val) #f]  
    [interior-node (left right)
                   (cond
                     [(and (equal? (positive-thinking left) #f)
                           (equal? (positive-thinking right) #f))
                      #f]
                     
                     [(and (not (equal? (positive-thinking left) #f))
                           (equal? (positive-thinking right) #f))
                      (positive-thinking left)]
                     
                     [(and (equal? (positive-thinking left) #f)
                           (not(equal? (positive-thinking right) #f)))
                      (positive-thinking right)]
                     
                     [(and (not(equal? (positive-thinking left) #f))
                           (not(equal? (positive-thinking right) #f)))
                      (interior-node (positive-thinking left) (positive-thinking right))])]))


(test(positive-thinking (negative-leaf 4))#f)
(test(positive-thinking (interior-node (interior-node (negative-leaf 16)
(positive-leaf 15))  (positive-leaf 3)))(interior-node (positive-leaf 15)(positive-leaf 3)))

(test(positive-thinking (interior-node (interior-node (negative-leaf 16)
(positive-leaf 15))  (interior-node (positive-leaf 19)
(positive-leaf 9))))(interior-node (positive-leaf 15) (interior-node (positive-leaf 19)
(positive-leaf 9))))

(test(positive-thinking (interior-node (interior-node (negative-leaf 16)
(positive-leaf 15))  (interior-node (negative-leaf 19)
(negative-leaf 9)))) (positive-leaf 15))

(test(positive-thinking (interior-node (interior-node (negative-leaf 16)
(negative-leaf 15))  (interior-node (negative-leaf 19)
(negative-leaf 9))))#f)

(test(deep-balanced? (interior-node (negative-leaf 1) (positive-leaf 1)))#t)
(test(deep-balanced? (interior-node(interior-node (negative-leaf 1) (positive-leaf 1))(negative-leaf 1)))#f)

(test(deep-balanced? (interior-node(interior-node (negative-leaf 1) (positive-leaf 1))(positive-leaf 1)))#f)