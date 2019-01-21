;Fairylog
;Copyright Ross McKinlay, 20179

#lang racket/base

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/match
                     racket/syntax
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
(define tabs 0)
(define (inc-tab) (set! tabs (+ tabs 1)))
(define (dec-tab) (set! tabs (- tabs 1)))

(define-syntax-parser prt
  [(_ str args ...)
   #'(printf (string-append (make-string tabs (integer->char 9)) str) args ...)])
(define-syntax-parser pr
  [(_ str args ...)
   #'(printf str args ...)])

(define (print-pad) (printf  (make-string tabs (integer->char 9))))


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
      #'(format "~a == ~a" (~expression x) (~expression y))]
  [(_ (set x y))
      #'(format "~a <= ~a" (~expression x) (~expression y))]
  [(_ x:id)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
      #'name]
  [(_ expr )
   #'"expr..."])

(define-syntax-parser ~cond
  #:datum-literals (else)
  [(_  [first-test first-outcome]  [expr-test expr-outcome] ...
       [else else-outcome])
   #`(begin
       (~cond
         [first-test first-outcome]
         [expr-test expr-outcome] ...)
       (prt "else begin\n") 
       (inc-tab)
       (prt "~a" (~expression else-outcome))
       (printf "; \n")
       (dec-tab)
       (prt "end\n")       
       )]
  [(_  [first-test first-outcome]  [expr-test expr-outcome] ...)
   #'(begin
       (prt "if(")
       (printf "~a" (~expression first-test))
       (printf ") begin\n")
       (inc-tab)
       (prt "~a" (~expression first-outcome))
       (printf "; \n")
       (dec-tab)
       (prt "end\n")
       (begin
         (prt "else if(")
         (printf "~a" (~expression expr-test))
         (printf ") begin\n")
         (inc-tab)
         (prt "~a" (~expression expr-outcome))
         (printf "; \n")
         (dec-tab)
         (prt "end\n")) ...                
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
  #:datum-literals (~cond ~locals ~expression)
  [(_ (~cond expr  ...))
   #'(~cond expr ...)]
  [(_ (~locals params ...))
   #'(~locals params ...)]
  [(_ (~expression expr ...))
     #'(begin
         (prt "~a;\n" (~expression expr ...)))]
  [(_ expr ...)
     #'(~begin-line (~expression expr ...))])


(define-syntax-parser ~begin
  [(_ block-name:id expr ...+)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'block-name)))
   #'(begin
       (prt "begin : ~a\n" name)
       (inc-tab)
       (~begin-line expr) ...
       (dec-tab)
       (prt "end //end of block ~a\n" name)
       )]
  [(_  expr ...+)
   #'(begin
       (prt "begin\n")
       (inc-tab)
       (~begin-line expr) ...
       (dec-tab)
       (prt "end //end of block\n")
       )])

(define-syntax-parser ~always-line
  [(_ expr) #'expr])
  ;; #:datum-literals (~begin)
  ;; [(_ (~begin name expr ...))
  ;;  #'(~begin name expr ...)])

(define-syntax-parser ~sync
  [(_ target rx clk)
   (with-syntax
     ([q_rx (string->symbol "q_rx")]   ;todo; gen symbol names
      [qq_rx (string->symbol "qq_rx")])
   #'(begin
       (~locals
        ([q_rx   #:reg] 
         [qq_rx  #:reg] 
         [target #:reg])) 
       (~always ([#:posedge clk])
         (~begin 
          (set q_rx rx)
          (set qq_rx q_rx)
          (set target qq_rx)))))])
                
              
(define-syntax-parser ~locals
  [(_ (params:local-param ...))
   #'(begin
       (prt "~a ~a ~a ~a;\n" params.type params.size params.name params.default) ...       
       )])
  
(define-syntax-parser ~always
  #:datum-literals (* or)
  [(_ (or sens:sensitivity rest:sensitivity ...) expr ...)
   #'(begin
       (prt "always @(")
       (begin
         (pr "~a" sens.edge-type)
         (pr " ~a" sens.name)) 
       (Begin
         (pr " or ")
         (pr "~a" rest.edge-type)
         (pr " ~a"rest.name)) ...
         (pr ")\n")
       (inc-tab)
       (~always-line expr ...)
       (dec-tab)
       )]
  [(_ (sens:sensitivity rest:sensitivity ...) expr ...)
   #'(begin
       (prt "always @(")
       (begin
         (pr "~a" sens.edge-type)
         (pr " ~a" sens.name)) 
       (begin
         (pr " , ")
         (pr "~a" rest.edge-type)
         (pr " ~a"rest.name)) ...
       (pr ")\n")
       (inc-tab)
       (~always-line expr ...)
       (dec-tab)
       )])
    
(define-syntax-parser ~module-line
  [( _ x) #'x])
  ;; #:datum-literals (~begin ~always ~locals)
  ;; [(_ (~begin name expr ...))
  ;;  #'()]
  ;; [(_ (~always sens-list expr ...))
  ;;  #'(~always sens-list expr ...)]
  ;; [(_ (~syn stuff ...))
  ;;     #'(~sync stuff ...)]                       

  ;; )

(define-syntax-parser ~module
  [(_ name-sym:id
      (p:param ... last:param)
      expression ... )

   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
  #'(begin
      (prt "module ~a (\n" name)
      (inc-tab)
      ;port declarations
      (prt "~a ~a ~a ~a,\n" p.direction p.type p.size p.name) ...
      (prt "~a ~a ~a ~a);\n" last.direction last.type last.size last.name)
      
      (~module-line expression) ...
      (dec-tab)
      (prt "endmodule\n"))])
  
  
(provide (all-defined-out))
