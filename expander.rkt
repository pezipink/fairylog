;Fairylog
;Copyright Ross McKinlay, 20179

#lang racket/base

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/set
                     racket/list
                     racket/match
                     racket/syntax
                     racket/string
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
  (define is-always-sens #f)  
  (define (toggle-always-sens)
    (set! is-always-sens (not is-always-sens)))
  (define declared-enums (make-hash))
  (define (enum-exists? enum-name)
    (hash-has-key? declared-enums enum-name))
  (define (enum-key-exists? enum-name key)
    (member (symbol->string key)
            (map car (hash-ref declared-enums (symbol->string enum-name)))))
  (define (get-enum-keys enum-name)
    (map car (hash-ref declared-enums (symbol->string enum-name))))
  (define (get-enum-value enum-name key)
    (printf "eg ~a ~a\n" enum-name key)
    (let* ([pairs (hash-ref declared-enums (symbol->string enum-name))]
           [pair (memf (λ (p)
                         (printf "~a = ~a\n" (car p) (symbol->string key))
                         (equal? (car p) (symbol->string key))) pairs)])
      (printf "ret ~a \n" (cdr (car pair)))
      (cdr (car pair))))
  (define (add-enum enum-name vals)
    (hash-set! declared-enums (symbol->string enum-name) vals))

  (define-syntax-class enum
    #:description "a declared enum"
    #:opaque
    (pattern x:id #:when (enum-exists? (symbol->string (syntax-e (attribute x))))))

  (define-syntax-class enum-kvp
    #:description "a name and numeric value pair"
    #:opaque
    (pattern [x:id y]
             #:with y-evaled (eval (syntax-e (attribute y)))
             #:with pair (cons
                           (format "~a" (syntax-e (attribute x)))
                           (syntax-e (attribute y-evaled)))

             ))
  
  (define scoped-bindings-stack (box (list (mutable-set))))
  (define (push-scoped-stack)
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cons (mutable-set) lst)])
      (set-box! scoped-bindings-stack new-lst)))
    
  (define (pop-scoped-stack)
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cdr lst)])
      (set-box! scoped-bindings-stack new-lst)))

  (define (peek-scoped-stack)
    (let ([lst (unbox scoped-bindings-stack)])
      (car lst)))

  (define (add-scoped-binding stx-name stx)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
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
    (aux (unbox scoped-bindings-stack)))

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


  (define-syntax-class bound-usage
    #:description "identifier in scope with or without size"
    #:opaque
    (pattern s:scoped-binding
             #:with name #'s.name
             #:with size (datum->syntax this-syntax "")
             #:with compiled (datum->syntax this-syntax (symbol->string (syntax-e (attribute s)))))
    
    (pattern [s:scoped-binding x:expr y:expr]
             #:with name #'s.name
             #:with size
             (datum->syntax this-syntax
                            (format "[~a:~a]"
                                    (eval (syntax-e (attribute x)))
                                    (eval (syntax-e (attribute y)))))
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a[~a:~a]"
                                    (syntax-e (attribute name))
                                    (eval (syntax-e (attribute x)))
                                    (eval (syntax-e (attribute y)))))))


  
  )

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

(define-syntax (toggle-always-sens stx)
  (syntax-parse stx
    [(_)
     (toggle-always-sens)
  #''()]))
  
(begin-for-syntax
  (define (is-hex-literal? str)
    (regexp-match #px"^[$][0-9A-Fa-f_ZzXx]+$" str))

  (define (is-binary-literal? str)
    (regexp-match #px"^[%][01_ZzXx]+$" str))

  (define (is-hex-string? str)
    (regexp-match #px"^[0-9A-Fa-f_ZzXx]+$" str))

  (define (is-binary-string? str)
    (regexp-match #px"^[$][01_ZzXx]+$" str))

  (define (is-number-literal-candidate? str)
    ;todo: need better literal checking
    ; eg check literal with base is not greater than size
    ; check literals characters properly - binary only 01xz_ etc
    (let ([parsed
           (regexp-match #px"^([0-9]+)_(2|8|10|16)_(-)?([0-9A-Fa-f_ZzXx]+$)" str)])
      (if (eq? parsed #f)
          #f
          (cdr parsed)))) ; outputs size base negative? value



  (define-syntax-class number-literal
    #:datum-literals (_)
    (pattern x:integer
             #:with base 10
             #:with bits 0 ;todo calc?
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a" (syntax-e (attribute x)))))
    ;hex literals
    (pattern x:id
             #:do [(define str (symbol->string (syntax-e (attribute x))))]
             #:when (is-hex-literal? str)
             #:do [(define cleaned (string-replace
                                    (string-replace str "_" "") "$" ""))]
             #:with base 16
             #:with bits (* 4 (string-length cleaned) 1)
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a'h~a"
                                    (syntax-e (attribute bits))
                                    (substring str 1))))
    ;binary literals
    (pattern x:id
             #:do [(define str (symbol->string (syntax-e (attribute x))))]
             #:when (is-binary-literal? str)
             #:do [(define cleaned (string-replace
                                    (string-replace str "_" "") "%" ""))]
             #:with base 2
             #:with bits (- (string-length str) 1)
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a'b~a"
                                    (syntax-e (attribute bits))
                                    (substring str 1) )))
    ;full literal syntax
    (pattern x:id
             #:do [(define str
                     (is-number-literal-candidate?
                      (symbol->string (syntax-e (attribute x)))))]
             #:when (list? str)
             #:with base 
             (case (string->number (list-ref str 1))
               [(2)  "'b"]
               [(8)  "'o"]
               [(10) "'d"]
               [(16) "'h"])
             #:with bits  (string->number (list-ref str 0))
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a~a~a~a"
                                    (case (list-ref str 2)
                                      [(#f) ""]
                                      [else "-"])
                                    (syntax-e (attribute bits))
                                    (syntax-e (attribute base))
                                    (list-ref str 3)
                                              )))
             )



  
      
  (define-syntax-class edge-type
    (pattern #:posedge)
    (pattern #:negedge))

  (define-syntax-class sensitivity
    (pattern [edge:edge-type signal:bound-usage]
             #:with edge-type (datum->syntax this-syntax (keyword->string (syntax-e #'edge)))
             #:with compiled #'signal.compiled)
    (pattern [signal:id]             
             #:with edge-type (datum->syntax this-syntax "")
                          #:with compiled #'signal.compiled))
;             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'signal)))))

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
              (~optional [x (~optional y)])]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with direction (datum->syntax this-syntax (keyword->string (syntax-e #'direction-option)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with size
             (cond
               [(and (attribute x) (attribute y))
                 (datum->syntax this-syntax
                                (format "[~a:~a] "
                                        (syntax-e (attribute x))
                                        (syntax-e (attribute y))))]
               [(attribute x)
                (datum->syntax this-syntax
                                (format "[~a:~a] "
                                        (- (eval (syntax-e (attribute x))) 1)
                                        0))]
               [else
                 (datum->syntax this-syntax "")])))
    (define-syntax-class local-param
    #:datum-literals (:)
    (pattern [name-sym:id
              type-option
              (~optional [x (~optional y)])
              (~optional default-value)]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with default
             (if (attribute default-value)
                 (datum->syntax this-syntax (format " = ~a" (eval (syntax-e #'default-value))))
                 (datum->syntax this-syntax ""))
             #:with size
             (cond
               [(and (attribute x) (attribute y))
                 (datum->syntax this-syntax
                                (format "[~a:~a] "
                                        (syntax-e (attribute x))
                                        (syntax-e (attribute y))))]
               [(attribute x)
                (datum->syntax this-syntax
                                (format "[~a:~a] "
                                        (+ 1 (eval (syntax-e (attribute x))))
                                        0))]
               [else
                 (datum->syntax this-syntax "")]))))

(define-syntax-parser ~expression
  #:datum-literals (~eq? ~set ~delay ~+ ~- ~if ~case else )
  [(_ x:integer)
   #'x]
  [(_ x:number-literal)
   #'x.compiled]

  [(_ (~delay x y))
   #'(format "#~a ~a" (~expression x)(~expression y))]
  [(_ (~if test true-expr false-expr))
   #'(format "~a ? ~a : ~a" (~expression test)(~expression true-expr)(~expression false-expr))]
  ;todo: error handling case when default is not supplied

  [(_ (~case val
             [test true-expr]
             [test2 expr2] ...+
             [else def-expr]))
   #'(format "~a ? ~a : ~a"
             (~expression (~eq? val test))
             (~expression true-expr)
             (~expression (~case val [test2 expr2] ... [else def-expr])))
   ]
  [(_ (~case val [test true-expr]
                 [else def-expr]))
   #'(format "~a ? ~a : ~a"
             (~expression (~eq? val test))
             (~expression true-expr)
             (~expression def-expr))]
  [(_ (~case val [test true-expr] ...+))
   #:fail-when #t "you must supply an else branch of a case when used as an epxression"
   #'(void)]

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
  [(_ (~set x y))
   #:when is-always-sens
   #'(format "~a <= ~a" (~expression x) (~expression y))]
  [(_ (~set x y))
   #'(format "~a = ~a" (~expression x) (~expression y))]
  [(_ x:bound-usage)
   #'x.compiled]
  [(_ x:binding)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
   #'(error x name "is not in scope")]

  ;; [(_ x:expr)
  ;;  #'x]
  
)

(define-syntax-parser ~begin-or-wrap-expression
  #:datum-literals (~begin)
  [(_ ~begin exprs ...)
   #'(~begin exprs ...)]
  [(_ x:expr)
   #'(~expression x)])

   
(define-syntax-parser ~case
  #:datum-literals (else)
  [(_ test:bound-usage [lhs:number-literal rhs] ...)
   #'`(
       tab
       "case ("
       ,test.compiled
       ")\n"
       inc-tab
       (
        tab
        ,lhs.compiled
        " : "
        ,(~begin-or-wrap-expression rhs)
        ";\n"
        ) ...
       dec-tab
       tab
       "endcase\n")])

(define-syntax-parser ~cond
  #:datum-literals (else)
  [(_  [first-test first-outcome]  [expr-test expr-outcome] ...
       [else else-outcome])
   (printf "cond\n")
        (printf "COND ORIGIN ~a\n" (syntax-property this-syntax 'origin ))
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
     (printf "COND ORIGIN ~a\n" (syntax-property this-syntax 'origin ))
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

(define-syntax-parser ~enum
  [(_ name kvp:enum-kvp ...+)
   
   #:fail-when (check-duplicate-identifier
                (syntax->list #'(kvp.x ...)))
    "duplicate enum name"
   #:fail-when (check-duplicates
                (syntax->datum #'(kvp.y-evaled ...)))
   "duplicate enum value"   
   (add-enum (syntax-e #'name) (syntax->datum #'(kvp.pair ...)))
   #'(void)])

(define-syntax-parser ~match-set
  [(_ target:bound-usage test:expr enum-name:enum
      [key value] ...)
   #:fail-when (check-duplicate-identifier (syntax->list #'(key ...)))
    "duplicate enum value"

   #:fail-when
   (let ([results (filter (λ (v) (not (enum-key-exists? (syntax-e #'enum-name) v)))
                          (syntax->datum #'(key ...)))])
     (if (not (eq? results '()))
         (with-syntax ([res results])  #'res)
       #f))
   "some identifiers do not exist in enum"

   #:fail-when
   (let*
       ([keys (map (λ (v) (format "~a" v)) (syntax->datum #'(key ...)))]
        [results (filter (λ (v) (not (member v keys)))
                         (get-enum-keys (syntax-e #'enum-name)))])
     (if (not (eq? results '()))
         (with-syntax ([res results]) #'res)
         #f))
   "missing cases in the enum"
     
   (with-syntax([(enum-vals ...) (map (λ (v) (get-enum-value (syntax-e #'enum-name) v))
                                      (syntax->datum #'(key ...)))])
     #'(~case test [enum-vals (~set target value)] ...))]
)

(define-syntax-parser case-set
  [(_ target:bound-usage test:expr
      [key:number-literal value] ...)     
     #'(~case test [key (~set target value)] ...)])

(define-syntax-parser ~begin-line
  #:datum-literals (~cond ~locals ~expression ~when ~if ~set ~match-set)  
  [(_ (~cond expr  ...))
   #'(~cond expr ...)]
  [(_ (~when expr  ...))
   #'(~when expr ...)]
  [(_ (~if expr  ...))
   #'(~if expr ...)]  
  [(_ (~locals params ...))
   #'(~locals params ...)]
  [(_ (~match-set expr ...))
      #'(~match-set expr ...)]
  [(_ (~expression expr ...))
     #'`(tab
         ,(format "~a;\n" (~expression expr ...)))]  
  [(_ (~set [x y] ...))
   #'`(
       (tab
            ,(format "~a;\n" (~expression (~set x y))))...)]
  [(_ expr ...)
        #'`(tab
         ,(format "~a;\n" (~expression expr ...)))
   ;#'(~begin-line (~expression expr ...))
   ])


(define-syntax-parser ~begin
  [(_ block-name:id expr ...+)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'block-name)))
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
   #'`(
       tab
       "begin\n"
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       "end //end of block\n"
       )])


(define-syntax-parser ~sync
  [(_ target rx clk)
   (with-syntax
     ([q_rx (string->symbol "q_rx")]   ;todo; gen symbol names
      [qq_rx (string->symbol "qq_rx")])
     (printf "sync\n")
   #'`(
       ,(~locals
        [q_rx   #:reg] 
         [qq_rx  #:reg] 
         [target #:reg]) 
       ,(~always ([#:posedge clk])
         (~begin 
          (~set q_rx rx)
          (~set qq_rx q_rx)
          (~set target qq_rx)))))])
                

(define-syntax-parser ~locals
  [(_ params:local-param ...)
   #'`(
       (
       tab
       ,(format "~a ~a ~a ~a;\n" params.type params.size params.name params.default)) ...
       ,(push-binding params.name) ...
       )])

(define-syntax-parser ~always-line  
  [(_ expr)
   (printf "always-line\n")
   (printf "always line ORIGIN ~a\n" (syntax-property this-syntax 'origin ))
   
   #'expr])
  ;; #:datum-literals (~begin)
  ;; [(_ (~begin name expr ...))
  ;;  #'(~begin name expr ...)])

(define-syntax-parser ~always
  #:datum-literals (* or)
  [(_ (or sens:sensitivity rest:sensitivity ...) expr ...)
   (printf "always\n")
   (toggle-always-sens)
   #'`(
       tab
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
      dec-tab
      ,(toggle-always-sens))]
  [(_ (sens:sensitivity rest:sensitivity ...) expr ...)
   (toggle-always-sens)
   #'`(
       tab
       "always @("
       ,(format "~a" sens.edge-type)
       ,(format " ~a" sens.compiled)
       (
         " , "
         ,(format "~a" rest.edge-type)
         ,(format " ~a"rest.name)) ...
       ")\n"
       inc-tab
       ,(~always-line expr ...)
       dec-tab
       ,(toggle-always-sens)
       )]
    [(_ * expr ...)
   (toggle-always-sens)
   #'`(
       tab
       "always @(*)\n"
       inc-tab
       ,(~always-line expr ...)
       dec-tab
       ,(toggle-always-sens)
       )]
    [(_ expr ...)
   #'`(
       tab
       "always\n"
       inc-tab
       ,(~always-line expr ...)
       dec-tab
       )])


    
(define-syntax-parser ~module-line
  #:datum-literals (~set) 
  [(_ (~set [x y] ...))
   #'`((tab
        ,(format "~a;\n" (~expression (~set x y))))...)]
  [(_ (~set x y))
   #'`(tab
       ,(format "~a;\n" (~expression (~set x y))))]
  [( _ x)
   #'x]
)
 


(define (code-gen input filename)
  (define tab 0)
  (define out (open-output-file #:mode 'binary #:exists 'replace filename))
  (define (aux in)
    (for ([sym in])
     
      (cond
        [(string? sym)
         (begin

           (display sym out))]
        [(eq? 'inc-tab sym) (set! tab (+ 1 tab))]
        [(eq? 'tab sym)     (display (make-string tab (integer->char 9)) out)]
        [(eq? 'dec-tab sym) (set! tab (- tab 1))]
        [(eq? '() sym) '()]
        [(list? sym) (aux sym)]
        [else (printf "unknonw ~a\n" sym)
              ])))
  (aux input)
  (close-output-port out))


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
      "\n"
      dec-tab
      ,(~module-line expression) ...

      "endmodule\n"
      ,(pop-scoped-stack)
      )])
  
  
(provide (all-defined-out))
