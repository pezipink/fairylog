;Fairylog
;Copyright Ross McKinlay, 2010
#lang racket/base
(require (rename-in racket                   
                    [cond r:cond]
                    [match r:match]
                    [when r:when]
                    [if r:if]
                    [case r:case]
                    [begin r:begin]
                    [eq? r:eq?]))
(require (for-syntax racket 
;"expander.rkt"
))
(require (rename-in "expander.rkt"
                    [~cond cond]
                    [~match match]
                    [~begin begin]
                    [~case case]
                    [~if if]
                   [~when when]
                    ))

(provide ;
 #%module-begin
 (except-out (all-from-out racket)
             ;cond
             )
 (for-syntax  (all-from-out racket)
;              (all-from-out "expander.rkt")
)
 
  (all-from-out "expander.rkt"))
 
