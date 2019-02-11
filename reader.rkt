;fairylog
;Copyright Ross McKinlay, 2019

#lang racket
(require syntax/readerr)

(provide wrapper1
         make-fairylog-readtable)

;; make-no-bar-readtable : [(U #false Readtable)] -> Readtable
(define (make-no-vert-bar-readtable [rt (current-readtable)])
  (make-readtable rt
    #\|  ; The vertical bar will behave
    #\a  ; the same way a normal character (such as a)
    #f)) ; behaves in the default readtable.

(define (make-fairylog-readtable)
  (make-no-vert-bar-readtable))
  
(define (wrapper1 thk)
  (parameterize ([current-readtable (make-fairylog-readtable)])
    (thk)))
