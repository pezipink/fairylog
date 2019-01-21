;fairylog
;Copyright Ross McKinlay, 2019

#lang racket
(require syntax/readerr)

(provide wrapper1
         make-fairylog-readtable)

(define (make-fairylog-readtable)
  (current-readtable))
  
(define (wrapper1 thk)
  (parameterize ([current-readtable (make-fairylog-readtable)])
    (thk)))
