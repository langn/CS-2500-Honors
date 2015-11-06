;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Problem Set 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set 7h

(require racket/list)

;; Problem 1



;; Problem 2

; A [PartialListTree X] ([PLT X]) is one of:
; - 'leaf
; - (make-node Natural X [PLT X] [PLT X])
(define-struct node (id val left right))
 
; A [PLT X] represents some of the values in a [Listof X] and their indices in
; the list
 
; IMPORTANT: Given a PLT p, every node in (node-left p) has id < (node-id p)
; and every node in (node-right p) has id > (node-id p)

(define PLT1 (make-node 0 "Yes" 'leaf 'leaf)) ; A [PLT String]
(define PLT2 (make-node 5 'hello (make-node 2 'abc 'leaf 'leaf) 'leaf))
; A [PLT Symbol]
(define PLT3 (make-node 5 16 ; A [PLT Number]
                        (make-node 2 20 'leaf 'leaf)
                        (make-node 10 5 'leaf (make-node 20 0 'leaf 'leaf))))

;; update : [PLT X] Natural X -> [PLT X]
;; If the given PLT has the id i in it, then we update the value at that id to
;; be v.  If the PLT does not have the id i in it, we will add in the id, value
;; pair (i,v) at the appropriate locatoin.
(define (update plt i v)
  (cond
    [(= i (node-id plt)) (make-node (node-id plt)
                                    v
                                    (node-left plt)
                                    (node-right plt))]
    [(and (symbol? (node-left plt)) (< i (node-id plt)))
     (make-node (node-id plt)
                (node-val plt)
                (make-node i v 'leaf 'leaf)
                (node-right plt))]
    [(and (symbol? (node-right plt)) (> i (node-id plt)))
     (make-node (node-id plt)
                (node-val plt)
                (node-left plt)
                (make-node i v 'leaf 'leaf))]
    [(< i (node-id plt))
     (make-node (node-id plt)
                (node-val plt)
                (update (node-left plt) i v)
                (node-right plt))]
    [(> i (node-id plt))
     (make-node (node-id plt)
                (node-val plt)
                (node-left plt)
                (update (node-right plt) i v))]))

;; Tests
(check-expect (update PLT3 20 6)
              (make-node 5 16 (make-node 2 20 'leaf 'leaf)
                         (make-node 10 5 'leaf (make-node 20 6 'leaf 'leaf))))
(check-expect (update PLT3 6 20)
              (make-node 5 16 (make-node 2 20 'leaf 'leaf)
                         (make-node 10 5 (make-node 6 20 'leaf 'leaf)
                                    (make-node 20 0 'leaf 'leaf))))
(check-expect (update PLT3 16 20)
              (make-node 5 16 (make-node 2 20 'leaf 'leaf)
                         (make-node 10 5 'leaf
                                    (make-node 20 0
                                               (make-node 16 20 'leaf 'leaf)
                                               'leaf))))

;; Problem 3

;; A [ReturnValue X] ([RT X]) is one of:
;; - false
;; - X
(define RT1 false)
(define RT2 "Yes") ; An [RT String]
(define RT3 'abc) ; An [RT Symbol]
(define RT4 5) ; An [RT Nubmer]

;; retrieve : [PLT X] Natural -> [RT X]
;; If the given PLT has the specified id return the value of the that id.  Else,
;; return false.
(define (retrieve plt i)
  (cond
    [(symbol? plt) false]
    [(= i (node-id plt)) (node-val plt)]
    [(< i (node-id plt)) (retrieve (node-left plt) i)]
    [(> i (node-id plt)) (retrieve (node-right plt) i)]))

;; Tests
(check-expect (retrieve PLT3 5) 16)
(check-expect (retrieve PLT3 2) 20)
(check-expect (retrieve PLT3 10) 5)
(check-expect (retrieve PLT3 20) 0)
(check-expect (retrieve PLT3 7) false)

;; Problem 4

; A [Tree X] is one of:
; - 'leaf
; - (make-tree X [Tree X] [Tree X])
(define-struct tree (val left right))

;; examples
(define TREE1 (make-tree 2
                         (make-tree 1 'leaf 'leaf)
                         (make-tree 4
                                    (make-tree 3 'leaf 'leaf)
                                    (make-tree 5 'leaf 'leaf))))
(define TREE2 (make-tree "bob"
                         (make-tree " ross"
                                    (make-tree " is " 'left 'leaf) 'leaf)
                         (make-tree "the best" 'leaf 'leaf)))


;; filter-tree : [Tree X] [X -> Boolean] -> [Listof X]
;; Produces a [Listof X] which contains all the elements of the tree which
;; produce true for the given predicate.
(define (filter-tree t pred)
  (flatten (cond
             [(symbol? t) empty]
             [(tree? t) (if (pred (tree-val t))
                            (list (filter-tree (tree-left t) pred)
                                  (tree-val t)
                                  (filter-tree (tree-right t) pred))
                            (list (filter-tree (tree-left t) pred)
                                  (filter-tree (tree-right t) pred)))])))

(check-expect (filter-tree TREE1 even?) (list 2 4))
(check-expect (filter-tree TREE2 (λ(n) (> (string-length n) 3)))
              (list " is " " ross" "the best"))
(check-expect (filter-tree 'leaf even?) (list))



;; Problem 5

;; fold-tree : [Tree X] [X Y Y -> Y] Y -> Y
;; Folds over the elements of the tree in the same way that foldr folds over
;; the elements of a list.
(define (fold-tree t f base)
  (cond
    [(symbol? t) base]
    [(tree? t) (f (tree-val t)
                  (fold-tree (tree-left t) f base)
                  (fold-tree (tree-right t) f base))]))

;; Tests
(check-expect (fold-tree TREE1 + 0) 15)
(check-expect (fold-tree TREE2 string-append "") "bob ross is the best")
(check-expect (fold-tree 'leaf * 1) 1)
                             
;; Problem 6

;; tree->list : [Tree X] -> [List X]
;; Produces a list of every item in left to right order
(define (tree->list t)
  (filter-tree t (λ(n) true)))

;; Tests
(check-expect (tree->list TREE1) (list 1 2 3 4 5))
(check-expect (tree->list TREE2) (list " is " " ross" "bob" "the best"))
(check-expect (tree->list 'leaf) (list))

