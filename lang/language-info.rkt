;Fairylog
;Copyright Ross McKinlay, 2019

#lang racket/base

(provide get-language-info)

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#[fairylog/lang/runtime-config configure #f])]
      [else default])))