#lang racket

(define (flatmap f lol)
  (compose flatten map))

(define (random-elt choices)
  (list-ref choices (random (length choices))))

(define grammar
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)))

(define (rule-lhs rule)
  (car rule))
(define (rule-rhs rule)
  (cdr (cdr rule)))
(define (rewrites category)
  (rule-rhs (assoc category grammar)))

;decided to use regular map and then flatten.
;instead of flatmap/mappend but either works.
(define (generate phrase)
  (cond
    [(list? phrase)
     (map generate phrase)]
    [(assoc phrase grammar)
     (flatten
       (generate (random-elt (rewrites phrase))))]
    [else (list phrase)]))
