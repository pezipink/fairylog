;Fairylog
;Copyright Ross McKinlay, 2019

#lang s-exp syntax/module-reader
fairylog
#:wrapper1 wrapper1
#:language-info #(fairylog/lang/language-info get-language-info #f)

(require "../reader.rkt")