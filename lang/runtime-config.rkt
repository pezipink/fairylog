;Fairylog                       `
;Copyright Ross McKinlay, 2017

#lang racket/base

(provide configure)

(require (only-in fairylog/reader make-fairylog-readtable))

(define (configure data)
  (current-readtable (make-fairylog-readtable)))