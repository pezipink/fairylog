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
 ;                   [module r:module]
                    [eq? r:eq?]))
(require (for-syntax racket))
;(require racket/syntax)
(require (rename-in "expander.rkt"
                    [~cond cond]
                    [~match match]
                    [~begin begin]
                    [~case case]
;                    [~module module]
                    [~if if]
                   [~when when]
                    ;[~eq? eq]
                    ))

(provide ;
 #%module-begin
 (except-out (all-from-out racket)
             ;cond
             )
 (for-syntax  (all-from-out racket))
 ;; (combine-out
 ;;     (all-from-out racket/syntax)
 ;;  (except-out
 ;;   (all-from-out racket/base)
 ;;   cond;match
 ;;   )
  (all-from-out "expander.rkt"))
 
