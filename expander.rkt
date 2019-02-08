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
  ; true when expanding inside an always block with a sensitivity list
  (define is-always-sens #f) 
  
  (define (toggle-always-sens)
    (set! is-always-sens (not is-always-sens)))
  
  (define declared-enums (make-hash))
  
  (define (enum-exists? enum-name)
    (hash-has-key? declared-enums enum-name))
  
  (define (enum-key-exists? enum-name key)
    (let ([enum-name
           (if (symbol? enum-name)
               (symbol->string enum-name)
               enum-name)]
          [key
           (if (symbol? key)
               (symbol->string key)
               key)])      
      (member key (map car (hash-ref declared-enums enum-name)))))

  (define (get-enum-keys enum-name)
    (map car (hash-ref declared-enums (symbol->string enum-name))))

  (define (get-enum-value enum-name key)    
    (let* ([enum-name
           (if (symbol? enum-name)
               (symbol->string enum-name)
               enum-name)]
          [key
           (if (symbol? key)
               (symbol->string key)
               key)]
           [pairs (hash-ref declared-enums enum-name)]
           [pair (memf (λ (p) (equal? (car p) key)) pairs)])
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
                           (syntax-e (attribute y-evaled)))))

  (define-syntax-class enum-literal
    #:description "enum literal in the form enum.value"
    (pattern x:id
             #:do
             [(define split
                (string-split
                 (symbol->string (syntax-e (attribute x)))
                 "."))]
             #:when (eq? (length split) 2 )
             #:fail-unless (enum-exists? (car split))
             (format "the enum ~a does not exist" (car split))
             #:fail-unless (enum-key-exists? (car split) (car (cdr split)))
             (format "the value ~a does not exist for enum ~a"
                     (car (cdr split))
                     (car split))
             #:with value (datum->syntax this-syntax (get-enum-value (car split) (car (cdr split))))
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a (~a)" (symbol->string (syntax-e (attribute x)))
                                    (get-enum-value (car split) (car (cdr split)))))
             #:with bits (datum->syntax this-syntax (string-length (format "~b" (get-enum-value (car split) (car (cdr split))))))
             ))

  
  (define scoped-bindings-stack (box (list (make-hash))))
  (define (push-scoped-stack)
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cons (make-hash) lst)])
      (set-box! scoped-bindings-stack new-lst)))
    
  (define (pop-scoped-stack)

    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cdr lst)])
      (set-box! scoped-bindings-stack new-lst)))

  (define (peek-scoped-stack)
    (let ([lst (unbox scoped-bindings-stack)])
      (car lst)))

  (define (add-scoped-binding stx-name stx-size stx)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (when (and (in-scope? name) (not (equal? name "global")))
        (writeln
         (format "warning: ~a is already in scope at ~a"
                 name (source-location->string stx))))
      (hash-set! scoped name stx-size)))

  (define (remove-scoped-binding stx-name)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (hash-remove! scoped name)))

  (define (in-scope? name)
  ;  (printf "in scope ~a\n" name)
    (define (aux lst)
      (cond
        [(empty? lst) #f]
        [(hash-has-key? (car lst) name) #t]
        [else (aux (cdr lst))]))
    (aux (unbox scoped-bindings-stack)))

  (define (get-binding-size name)
    (let ([name2 (if (syntax? name) (symbol->string (syntax-e name)) name)])
      (define (aux lst)

      (cond
        [(empty? lst)
         (begin
           'none)]
        [(hash-has-key? (car lst) name2)
         (begin
           (hash-ref (car lst) name2))]
        [else (aux (cdr lst))]))
      (aux (unbox scoped-bindings-stack))))

  (define-syntax-class scoped-binding
    #:description "identifier in scope"

    (pattern x:id
             #:with name  (symbol->string (syntax-e #'x))
             #:with name-stx (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
             #:when (in-scope? (symbol->string (syntax-e #'x)))
             #:with size-int (get-binding-size (symbol->string (syntax-e #'x)))
             ))

  (define-syntax-class binding
    #:description "identifier name"
    #:opaque
    (pattern x:id
             #:with name (symbol->string (syntax-e #'x))))

  (define-syntax-class bound-usage
    #:description "identifier in scope with or without size"

    (pattern s:scoped-binding
             #:with name #'s.name
             #:with size (datum->syntax this-syntax "")
             #:with size-int #'s.size-int
             #:with compiled   (datum->syntax this-syntax (symbol->string (syntax-e (attribute s))))
             #:with name-stx #'compiled) ;used in error reporting

    (pattern [s:scoped-binding x:scoped-binding]
             #:with name #'s.name
             #:with size-int #'1  ; indexing a sngle bit
             #:with compiled             
             #'`(name "[" x.name "]")
             #:with name-stx #'compiled) 

     (pattern [s:scoped-binding x:expr]
              #:with name #'s.name-stx
              #:with size-int #'1 ; indexing a single bit
              #:with compiled
              #'`(name "[" ,x "]")
              #:with name-stx #'`(name "[" x "]" )) 
     
    (pattern [s:scoped-binding x:expr y:expr]
             #:with name #'s.name
             #:with size-int #'x
             #:with compiled
             #'`(name "[" ,x ":" ,y "]")
             #:with name-stx #'compiled)) 
  
  )

(define-syntax (push-binding stx)
  (syntax-parse stx
    [(_ id size)
     (add-scoped-binding #'id #'size stx)
  #'(void)]))

(define-syntax (pop-scoped-stack stx)
  (syntax-parse stx
    [(_)
;     (printf "! ~a\n" stx)
     (pop-scoped-stack)
  #'(void)]))

(define-syntax (toggle-always-sens stx)
  (syntax-parse stx
    [(_)
     (toggle-always-sens)
  #'(void)]))


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


  (define (string-replace-many str from to)
    (for/fold ([str str])
              ([f from])
      (string-replace str f to)))

  (define-syntax-class number-literal
    #:datum-literals (_)
    (pattern x:integer
             #:with base 10
             #:with bits
             (datum->syntax this-syntax
               (string-length (format "~b" (syntax-e (attribute x))))) ;easy way out!
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
             ; for hex, leading zeroes are counted towards the length
             #:with bits (datum->syntax this-syntax (* 4 (string-length cleaned)))
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
             ; for binary, leading zeroes are counted towards the length
             #:with bits (datum->syntax this-syntax (string-length cleaned))
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
             #:do [(define radix (string->number (list-ref str 1)))                     
                   (define radix-str
                     (case (string->number (list-ref str 1))
                       [(2)  "'b"]
                       [(8)  "'o"]
                       [(10) "'d"]
                       [(16) "'h"]))
                   (define size (string->number (list-ref str 0)))
                   (define literal (list-ref str 3))]
             #:with base radix-str
             #:with bits size
             #:do [(let* ([n (string-replace-many literal '["X" "x" "Z" "z"]"0")]
                          [l
                           ;for all but decimal we count the leading zeroes as well
                           (case radix
                             [(2) (string-length n)]
                             [(8) (* (string-length n) 3)]
                             [(16) (* (string-length n) 4)]
                             [(10) (string-length (format "~b" (string->number n 10))
)])])
                     (when (> l size)
                       (printf "\"warning: number literal ~a does not fit into the specified size at ~a\"\n"
                               (symbol->string (syntax-e (attribute x))) #'x)))]
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a~a~a~a"
                                    (case (list-ref str 2)
                                      [(#f) ""]
                                      [else "-"])
                                    size radix-str literal))))
  
      
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
              (~optional [x (~optional y)])
              (~optional default-value)]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with direction (datum->syntax this-syntax (keyword->string (syntax-e #'direction-option)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with default
             (if (attribute default-value)
                 #'`(" = " ,(~expression default-value))
                 #'"")
             #:with size-int
             (cond
               [(and (attribute x) (attribute y))
                #'(+ (- x y) 1)]
               [(attribute x)
                #'x]
               [else #'1])
             #:with size
             (cond
               [(and (attribute x) (attribute y))
                #'`("[" ,x ":" ,y "]")]
               [(attribute x)
                #'`("[" ,(- x 1) ":0"  "]")]
               [else #'""])))

  (define-syntax-class local-param
    #:datum-literals (: array)
   (pattern [name-sym:id
              type-option
              (~optional [x (~optional y)])
              (~optional (array x2:expr (~optional y2)))]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with default
             (cond
               [(and (attribute x2) (attribute y2))
                #'`("[" ,x2 ":" ,y "2]")]
               [(attribute x2)
                #'`("[" ,(- x2 1) ": 0" "]")]
               [else #'""])                     
             #:with size-int
             (cond
               [(and (attribute x) (attribute y))
                #'(+ (- x y) 1)]
               [(attribute x)
                #'x]
               [else #'1])
             #:with size
             (cond
               [(and (attribute x) (attribute y))
                #'`("[" ,x ":" ,y "]")]
               [(attribute x)
                #'`("[" ,(- x 1) ": 0" "]")]
               [else #'""]))

   (pattern [name-sym:id
              type-option
              (~optional [x (~optional y)])
              (~optional
               default-value:expr)]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-option)))
             #:with default
                     
             (if (attribute default-value)
                 #'`(" = " ,(~expression default-value))
                 #'"")
             #:with size-int
             (cond
               [(and (attribute x) (attribute y))
                #'(+ (- x y) 1)]
               [(attribute x)
                #'x]
               [else #'1])
             #:with size
             (cond
               [(and (attribute x) (attribute y))
                #'`("[" ,x ":" ,y "]")]
               [(attribute x)
                #'`("[" ,(- x 1) ": 0" "]")]
               [else #'""]))


     
))
         

(define-syntax-parser ~expression
  #:datum-literals
  (~eq? ~set ~delay ~if ~case else ~when ~concat
   ~+ ~-  ~<< ~>>)
  [(_ x:integer)
   #'x]
  [(_ x:number-literal )
   #'x.compiled]
  [(_ (~delay x y))
   #'`("#" ,(~expression x) " " ,(~expression y))]
  [(_ (~when test true-expr))
   ;special case one-line when in RHS of expression - ternary
   #'(~begin (~when test true-expr))]
  [(_ (~concat x y ...+))
   #'`("{" ,(~expression x)  ( ", ",(~expression y)) ... "}" )]
  [(_ (~if test true-expr false-expr))
   #'`(
       ,(~expression test)
       " ? "
       ,(~expression true-expr)
       " : "
       ,(~expression false-expr))]

  [(_ (~case val
             [test true-expr]
             [test2 expr2] ...+
             [else def-expr]))
   #'`("~a ? ~a : ~a"              
       ,(~expression (~eq? val test))
       " ? "
       ,(~expression true-expr)
       " : "
       ,(~expression (~case val [test2 expr2] ... [else def-expr])))]
  [(_ (~case val [test true-expr]
                 [else def-expr]))
   #'`(
       ,(~expression (~eq? val test))
       " ? "
       ,(~expression true-expr)
       " : "
       ,(~expression def-expr))]
  [(_ (~case val [test true-expr] ...+))
   ;todo: error handling case when default is not supplied
   ;this case will not work when the last case (any expression) is enbaled.
   #:fail-when #t "you must supply an else branch of a case when used as an epxression"
   #'(void)]
  [(_ (~<< x y))
   #'`(
       ,(~expression x)
       " << "
       ,(~expression y))]
  [(_ (~>> x y))
   #'`(
       ,(~expression x)
       " >> "
       ,(~expression y))]
  [(_ (~+ x y z ...+))
   #'`(
       "("
       ,(~expression x)
       " + "
       ,(~expression (~+ y z ...))
       ")")]
  [(_ (~+ x y))
      #'`(,(~expression x)
       " + "
       ,(~expression y))]
  [(_ (~- x y z ...+))
   #'`(
       "("
       ,(~expression x)
       " - "
       ,(~expression (~- y z ...))
       ")")]
  [(_ (~- x y))
   #'`(,(~expression x)
       " - "
       ,(~expression y))] 
  [(_ (~eq? x y))
   #'`(,(~expression x)
       " == "
       ,(~expression y))]

  [(_ (~set (~or x:scoped-binding x:bound-usage) y:number-literal))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(when (> y.bits x.size-int)
          (printf "\"warning: the literal ~a does not fit into ~a and will be truncated\"\n" y.compiled x.name-stx))       
       ,(~expression x)
       op
       ,(~expression y))]

  [(_ (~set (~or x:scoped-binding x:bound-usage) y:enum-literal))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(when (> y.bits x.size-int)
          (printf "\"warning: the enum literal ~a does not fit into ~a and will be truncated\"\n" y.compiled x.name-stx))       
       ,(~expression x)
       op
       ,(~expression y))]

  [(_ (~set (~or x:scoped-binding x:bound-usage) (~or y:scoped-binding y:bound-usage)))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(when (> y.size-int x.size-int)
          (printf "\"warning: the expression ~a does not fit into ~a and will be truncated\"\n" y.name-stx x.name-stx))       
       ,(~expression x)
       op
       ,(~expression y))]

  [(_ (~set (~or x:scoped-binding x:bound-usage) y:expr))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #:with name (datum->syntax this-syntax (format "~a" #'y))
   #'`(
       ,(when (and (number? (~expression y))(> (~expression y) x.size-int))
          (printf "\"warning: the expression ~a does not fit into ~a and will be truncated\"\n" name x.name-stx))       
       ,(~expression x)
       op
       ,(~expression y))]
    
  [(_ (~set x y))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(~expression x)
       op
       ,(~expression y))]
  [(_ x:bound-usage)
   #'x.compiled]
  [(_ x:enum-literal)
   #'x.value]
  ;; [(_ x:binding)
  ;;  #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
  ;;  #'(error x name "is not in scope")]

  [(_ x:expr)
   #'x]
  
  )


(define-syntax-parser ~begin-or-wrap-expression
  #:datum-literals (~begin ~set ~when ~cond)
  [(_ (~begin exprs ...))
   #'(~begin exprs ...)]
  [(_ ~begin exprs ...)
   #'(~begin exprs ...)]
  [(_ (~when exprs ...))
   #'(~begin (~when exprs ...))]
  [(_ (~cond exprs ...))
   #'(~cond exprs ...)]
  [(_ ~when exprs ...)
   #'(~when exprs ...)]    
  [(_ (~set [x y] ...))
   #'(~begin (~set x y) ...)]

  [(_ x:expr)
   #'`(
       ,(~expression x)
       )])

   
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
        " : \n"
        ,(~begin rhs)
        "\n"
        ) ...
       dec-tab
       tab
       "endcase\n")])

(define-syntax-parser ~cond
  #:datum-literals (else)
  [(_  [first-test first-outcome] [expr-test expr-outcome] ...
       [else else-outcome])
   #'`(
       ,(~cond
         [first-test first-outcome]
         [expr-test expr-outcome] ...)
       tab
       "else\n"
       inc-tab
       ,(~begin else-outcome)
       "\n"
       dec-tab
       )]
  [(_  [first-test first-outcome])
   #'`(
       tab
       "if("
       ,(~expression first-test)       
       ")\n"
       inc-tab       
       ,(~begin first-outcome)
       dec-tab
       "\n"
        )]
  [(_  [first-test first-outcome] [expr-test expr-outcome] ...)
   #'`(
       tab
       "if("
       ,(~expression first-test)       
       ")\n"
       inc-tab       
       ,(~begin first-outcome)
       "\n"
       dec-tab
       (tab
        "else if("
        ,(~expression expr-test)
        ")\n"
        inc-tab
        ,(~begin expr-outcome)
        "\n"
        dec-tab
        "\n") ...                
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

(define-syntax-parser ~match
  [(_ test:expr enum-name:enum
      [key expr] ...)
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
     #'(~case test [enum-vals expr] ...))]
)

(define-syntax-parser ~case-set
  [(_ target:bound-usage test:expr
      [key:number-literal value] ...)     
     #'(~case test [key (~set target value)] ...)])

(define-syntax-parser ~begin-line
  #:datum-literals (~cond ~locals ~expression ~when ~if ~set ~match-set ~match ~case-set)  
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
  [(_ (~match expr ...))
   #'(~match expr ...)]
  [(_ (~case-set expr ...))
      #'(~case-set expr ...)]
  [(_ (~expression expr ...))
   #'`(tab
       ,(~expression expr ...)
       ";\n")]  
  [(_ (~set [x y] ...))
   #'`(
       (tab
        ,(~expression (~set x y))
        ";\n")...)]
  [(_ (~set x y))
   #'`(
       (tab
        ,(~expression (~set x y))
        ";\n"))]
  ;; [(_ expr ...)
  ;;  (printf "~a\n" #'(expr ...))
  ;;  #'`(tab
  ;;      ,(~expression expr ...)
  ;;      ";\n")
  ;#'(~begin-line (~expression expr ...))
  [(_ x:expr) #'x]
   )

(define-syntax-parser ~inc
  [( _ x:scoped-binding)
   #'`(tab
       ,(~expression (~set x (~+ x 1)))
       ";\n"
       )])

(define-syntax-parser ~begin
  [(_ block-name:id expr ...+)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'block-name)))
   #'`(
       tab
       "beign "
       ,name
       "\n"
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       "end \n"
       )]
  [(_  expr ...+)
   #'`(
       tab
       "begin\n"
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       "end \n"
       )])

(define-syntax-parser ~locals
  [(_ params:local-param ...)
   #'`(
       (
        tab
        ,(push-binding params.name params.size-int) ...
        (
         ,params.type
         " "
         ,params.size
         " "
         ,params.name
         " "
         ,params.default
         ";\n") ...))])

(define-syntax-parser ~always-line  
  [(_ expr)
   ;; (printf "always-line\n")
   ;; (printf "always line ORIGIN ~a\n" (syntax-property this-syntax 'origin ))
   
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
       
       ,sens.edge-type
       " "
       ,sens.compiled
       (
         " or "
         ,rest.edge-type
         " "
         ,rest.name
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
       ,sens.edge-type
       " " 
       ,sens.compiled
       (
         " , "
         ,rest.edge-type
         " "
         ,rest.name) ...
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
        ,(~expression (~set x y))
        "a;\n") ...)]
  [(_ (~set x y))
   #'`(tab
       ,(~expression (~set x y))
       "b;\n")]
  [( _ x)
   #'x])
 
(define-syntax-parser ~module
  [(_ name-sym:id
      (p:param ... last:param)
      expression ... )
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
    (push-scoped-stack)
  #'`(
      ,(format "module ~a (\n" name)      
      inc-tab
      ;port declarations
      (tab
       ,p.direction
       " "
       ,p.type
       " "
       ,p.size
       " "
       ,p.name
       " "
       ,p.default
       ",\n") ...

      tab
      ,last.direction
      " "
      ,last.type
      " "
      ,last.size
      " "
      ,last.name
      " "
      ,last.default
      ");"
      ,(push-binding p.name p.size-int) ...
      ,(push-binding last.name last.size-int)
      "\n"
      dec-tab
      ,(~module-line expression) ...

      "endmodule\n"
      ,(pop-scoped-stack)
      )])
  
(define (code-gen input filename)
  (define tab 0)
  (define out (open-output-file #:mode 'binary #:exists 'replace filename))
  (define (aux in)
    (for ([sym in])
     
      (cond
        [(or (string? sym) (integer? sym))
         (begin
           (display sym out))]
        [(eq? 'inc-tab sym) (set! tab (+ 1 tab))]
        [(eq? 'tab sym)     (display (make-string (* 2 tab) #\ ) out)]
        [(eq? 'dec-tab sym) (set! tab (- tab 1))]
        [(eq? '() sym) '()]
        [(list? sym) (aux sym)]
        [(void? sym) '()]
        [else (printf "unknonw ~a\n" sym)
              ])))
  (aux input)
  (close-output-port out))  
(provide (all-defined-out))
