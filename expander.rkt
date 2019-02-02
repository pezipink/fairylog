;Fairylog
;Copyright Ross McKinlay, 20179

#lang racket/base

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/set
                     racket/match
                     racket/syntax
                     syntax/srcloc
                     racket/list))


;(provide #%top #%app #%datum #%top-interaction)

(require syntax/parse/define)
(define is-debug #t)

(define-syntax (wdb stx)  
  (syntax-parse stx
    [(_ text)
     #'(when is-debug
         (writeln text))]
  [(_ text args ...)
   #'(when is-debug
       (writeln (format text args ...)))]))



(begin-for-syntax
  (define scoped-bindings-stack (box (list (mutable-set))))
  (define (push-scoped-stack)
 ;
   (printf "push\n")
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cons (mutable-set) lst)])
      (set-box! scoped-bindings-stack new-lst)))
    
  (define (pop-scoped-stack)
;    (printf "pop\n")
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cdr lst)])
      (set-box! scoped-bindings-stack new-lst)))

  (define (peek-scoped-stack)
    (let ([lst (unbox scoped-bindings-stack)])
      (car lst)))

  (define (add-scoped-binding stx-name stx)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
 ;     (printf "got ~a\n" name)
      (when (and (in-scope? name) (not (equal? name "global")))
        (writeln
         (format "warning: ~a is already in scope at ~a"
                 name (source-location->string stx))))
      (set-add! scoped name)))

  (define (remove-scoped-binding stx-name)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (set-remove! scoped name)))

  (define (in-scope? name)
;    (printf "in-scope? ~a \n" name)
    (define (aux lst)
      (cond
        [(equal? name "global") #t]
        [(empty? lst) #f]
        [(set-member? (car lst) name) #t]
        [else (aux (cdr lst))]))
    (aux (unbox scoped-bindings-stack))))
    

(begin-for-syntax
  (define-syntax-class scoped-binding
    #:description "identifier in scope"
    #:opaque
    (pattern x:id
             #:with name  (symbol->string (syntax-e #'x))
             #:when (in-scope? (symbol->string (syntax-e #'x)))
             ))

  (define-syntax-class binding
    #:description "identifier name"
    #:opaque
    (pattern x:id
             #:with name (symbol->string (syntax-e #'x))
             ))

  (define-syntax-class edge-type
    (pattern #:posedge)
    (pattern #:negedge))
  (define-syntax-class sensitivity
    (pattern [edge:edge-type signal:id]
             #:with edge-type (datum->syntax this-syntax (keyword->string (syntax-e #'edge)))
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'signal))))
    (pattern [signal:id]             
             #:with edge-type (datum->syntax this-syntax "")
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'signal)))))
  (define-syntax-class direction-option
    (pattern #:input)
    (pattern #:output))
  (define-syntax-class type-option
    (pattern #:wire)
    (pattern #:reg))
  (define-syntax-class param
    #:datum-literals (:)
    (pattern [name-sym:id
              direction-option
              type-option
              (~optional [x y])]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with direction (datum->syntax this-syntax (keyword->string (syntax-e #'direction-option)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with size
             (if (and (attribute x) (attribute y))
                 (datum->syntax this-syntax
                                (format "[~a:~a] "
                                        (syntax-e (attribute x))
                                        (syntax-e (attribute y))))
                 (datum->syntax this-syntax ""))))
    (define-syntax-class local-param
    #:datum-literals (:)
    (pattern [name-sym:id
              type-option
              (~optional [x y])
              (~optional default-value)]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with default
             (if (attribute default-value)
                 (datum->syntax this-syntax (format " = ~a" (syntax-e #'default-value)))
                 (datum->syntax this-syntax ""))
             #:with size
             (if (and (attribute x) (attribute y))
                 (datum->syntax this-syntax
                                (format "[~a:~a] "
                                        (syntax-e (attribute x))
                                        (syntax-e (attribute y))))
                 (datum->syntax this-syntax "")))))

(define-syntax-parser ~expression
  #:datum-literals (~eq? set ~delay ~+ ~-)
  [(_ x:integer)
   #'x]
  [(_ (~delay x y))
   #'(format "#~a ~a" (~expression x)(~expression y))]
  [(_ (~+ x y z ...+))
   #'(format "(~a + ~a)" (~expression x) (~expression (~+ y z ...)))]
  [(_ (~+ x y))
   #'(format "~a + ~a" (~expression x) (~expression y))]
  [(_ (~- x y z ...+))
   #'(format "(~a - ~a)" (~expression x) (~expression (~- y z ...)))]
  [(_ (~- x y))
   #'(format "~a - ~a" (~expression x) (~expression y))]
  [(_ (~eq? x y))
   #'(format "~a == ~a" (~expression x)(~expression y))]
  [(_ (set x y))
   #'(format "~a <= ~a" (~expression x) (~expression y))]
  [(_ x:scoped-binding)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'x))) 
   #'name]
  [(_ x:binding)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
   #'(error x name "is not in scope")]
  
)

(define-syntax-parser ~cond
  #:datum-literals (else)
  [(_  [first-test first-outcome]  [expr-test expr-outcome] ...
       [else else-outcome])
   (printf "cond\n")
   #'`(
       ,(~cond
         [first-test first-outcome]
         [expr-test expr-outcome] ...)
       tab
       "else begin\n"
       inc-tab
       tab
       ,(format "~a" (~expression else-outcome))
       "; \n"
       dec-tab
       tab
       "end\n"
       )]
  [(_  [first-test first-outcome]  [expr-test expr-outcome] ...)
   (printf "cond 2\n")
   #'`(
       tab
       "if("
       ,(~expression first-test)       
       ") begin\n"
       inc-tab
       tab
       ,(~expression first-outcome)
       "; \n"
       dec-tab
       tab
       "end\n"
       (tab
        "else if("
        ,(~expression expr-test)
        ") begin\n"
        inc-tab
        tab
        ,(~expression expr-outcome)
        "; \n"
        dec-tab
        tab
        "end\n") ...                
         )])

(define-syntax-parser ~if
  [(_ test-expr true-expr false-expr)
   #'(~cond
      [test-expr true-expr]
      [else false-expr])])

(define-syntax-parser ~when
  [(_ test-expr true-expr)
   #'(~cond
      [test-expr true-expr])])

(define-syntax-parser ~begin-line
  #:datum-literals (~cond ~locals ~expression ~when ~if)  
  [(_ (~cond expr  ...))
     (printf "begin line 1\n")
   #'(~cond expr ...)]
  [(_ (~when expr  ...))
   #'(~when expr ...)]
  [(_ (~if expr  ...))
   #'(~if expr ...)]
  
  [(_ (~locals params ...))
     (printf "begin line 2\n")
   #'(~locals params ...)]
  [(_ (~expression expr ...))
     (printf "begin line 3\n")
     #'`(tab
         ,(format "~a;\n" (~expression expr ...)))]
  [(_ expr ...)
     (printf "begin line\n")
     #'(~begin-line (~expression expr ...))])


(define-syntax-parser ~begin
  [(_ block-name:id expr ...+)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'block-name)))
   (printf "begin\n")
   #'`(
       tab
       ,(format "begin : ~a\n" name)
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       ,(format "end //end of block ~a\n" name)
       )]
  [(_  expr ...+)
       (printf "begin\n")
   #'`(
       tab
       "begin\n"
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       "end //end of block\n"
       )])

(define-syntax-parser ~always-line

  [(_ expr)
   (printf "always-line\n")
   #'expr])
  ;; #:datum-literals (~begin)
  ;; [(_ (~begin name expr ...))
  ;;  #'(~begin name expr ...)])

(define-syntax-parser ~sync
  [(_ target rx clk)
   (with-syntax
     ([q_rx (string->symbol "q_rx")]   ;todo; gen symbol names
      [qq_rx (string->symbol "qq_rx")])
     (printf "sync\n")
   #'`(
       ,(~locals
        ([q_rx   #:reg] 
         [qq_rx  #:reg] 
         [target #:reg])) 
       ,(~always ([#:posedge clk])
         (~begin 
          (set q_rx rx)
          (set qq_rx q_rx)
          (set target qq_rx)))))])
                
(define-syntax (push-binding stx)
  (syntax-parse stx
    [(_ id)
     (add-scoped-binding #'id stx)
  #'`(())]))

(define-syntax (pop-scoped-stack stx)
  (syntax-parse stx
    [(_)
     (printf "! ~a\n" stx)
     (pop-scoped-stack)
  #''()]))

(define-syntax-parser ~locals
;  (printf "locals\n")
  [(_ (params:local-param ...))
   #'`(
       (
       tab
       ,(format "~a ~a ~a ~a;\n" params.type params.size params.name params.default)) ...
       ,(push-binding params.name) ...
       )])
  
(define-syntax-parser ~always
  #:datum-literals (* or)
  [(_ (or sens:sensitivity rest:sensitivity ...) expr ...)
   (printf "always\n")
   #'`(
       "always @("
       
       ,(format "~a" sens.edge-type)
       ,(format " ~a" sens.name)
       (
         " or "
         ,(format "~a" rest.edge-type)
         ,(format " ~a" rest.name)
       ) ...
       ")\n"
       inc-tab
      ,(~always-line expr ...)
       dec-tab)]
  [(_ (sens:sensitivity rest:sensitivity ...) expr ...)
   (printf "always\n")
   #'`(
       "always @("
       ,(format "~a" sens.edge-type)
       ,(format " ~a" sens.name)
       (
         " , "
         ,(format "~a" rest.edge-type)
         ,(format " ~a"rest.name)) ...
       ")\n"
       inc-tab
       ,(~always-line expr ...)
       dec-tab
       )])
    
(define-syntax-parser ~module-line

  [( _ x)
   (printf "moduel line\n")
   #'x])
  ;; #:datum-literals (~begin ~always ~locals)
  ;; [(_ (~begin name expr ...))
  ;;  #'()]
  ;; [(_ (~always sens-list expr ...))
  ;;  #'(~always sens-list expr ...)]
  ;; [(_ (~syn stuff ...))
  ;;     #'(~sync stuff ...)]                       

;; )

(define (code-gen input filename)
  (define tab 0)
  (define out (open-output-file #:mode 'binary #:exists 'replace filename))
  (define (aux in)
    (for ([sym in])
      (cond
        [(string? sym) (display sym out)]
        [(eq? 'inc-tab sym) (set! tab (+ 1 tab))]
        [(eq? 'tab sym)     (display (make-string tab (integer->char 9)) out)]
        [(eq? 'dec-tab sym) (set! tab (- tab 1))]
        [(eq? '() sym) '()]
        [(list? sym) (aux sym)]
        [else (printf "unknonw ~a\n" sym)
              ])))
  (aux input)
  (close-output-port out))

(define-syntax-parser ~test1
  [(_ somethings ...)
   (printf "test1\n")
   (push-scoped-stack)
   #'`("first\n"
       inc-tab
       tab
       "second"
       ,somethings ...
       ,(pop-scoped-stack))])

(define-syntax-parser ~test2
  [(_ somethings ...)
   (printf "test2\n")
   #'`("third"
       ,somethings ...)])
(define-syntax-parser ~test3
  [(_ somethings ...)
   (printf "test3\n")
   #'`(
       "fourth"
       ,(format "~a" somethings) ...
       ;,somethings ...
       )])

(define-syntax-parser ~module

  [(_ name-sym:id
      (p:param ... last:param)
      expression ... )

   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
;     (printf "moduel\n")
    (push-scoped-stack)
  #'`(
      ,(format "module ~a (\n" name)      
      inc-tab
      ;port declarations
      (tab
       ,(format "~a ~a ~a ~a,\n" p.direction p.type p.size p.name))...
      tab
      ,(format "~a ~a ~a ~a);\n" last.direction last.type last.size last.name)
      ,(push-binding p.name) ...
      ,(push-binding last.name)
      ,(~module-line expression) ...
      dec-tab
      "endmodule\n"
      ,(pop-scoped-stack)
      )])
  
  
(provide (all-defined-out))
