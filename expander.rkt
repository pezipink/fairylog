
;Fairylog
;Copyright Ross McKinlay, 2019

#lang racket/base

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/list
                     racket/syntax
                     racket/string
                     syntax/srcloc
                     syntax/location
                     racket/list))


(require syntax/parse/define syntax/location)

(begin-for-syntax
  ; true when expanding inside an always block with a sensitivity list
  (define is-always-sens #f) 
  
  (define (toggle-always-sens)
    (set! is-always-sens (not is-always-sens)))
  
  (define declared-enums (make-hash))

  (define current-module "")
  (define (set-current-module name)
;    (printf "setting current module ~a\n" name)
    (set! current-module name))

  (define (enum-exists? ctx enum-name)
    (printf "in enum exists ~a\n" enum-name)
    (let ([gn (datum->syntax ctx (string->symbol (string-append "global-enum-" enum-name)))])
      (if (syntax-local-value gn (λ () #f))
          (begin (printf "found enum\n")
                 #t)
          (begin (printf "no found enum\n")
                 (hash-has-key? declared-enums enum-name)))))
  
  (define (enum-key-exists? ctx enum-name key)
    (printf "enum key exists\n")
    (let ([enum-name
           (if (symbol? enum-name)
               (symbol->string enum-name)
               enum-name)]
          [key
           (if (symbol? key)
               (symbol->string key)
               key)])
      (let ([gn  (datum->syntax ctx (string->symbol (string-append "global-enum-" enum-name)))])
        (if (syntax-local-value gn (λ () #f))
            (member key (map car (syntax-local-value gn)))                    
            (member key (map car (hash-ref declared-enums enum-name)))))
))

  (define (get-enum-keys ctx enum-name)
    (map car (hash-ref declared-enums (symbol->string enum-name))))

  (define (get-enum-value ctx enum-name key)    
    (let* ([enum-name
           (if (symbol? enum-name)
               (symbol->string enum-name)
               enum-name)]
           [key
            (if (symbol? key)
                (symbol->string key)
                key)]
           [gn (datum->syntax ctx (string->symbol (string-append "global-enum-" enum-name)))])
      (if (syntax-local-value gn (λ () #f))
          (let
              ([pair (memf (λ (p) (equal? (car p) key)) (syntax-local-value gn))])
            (cdr (car pair)))
          (let*
           ([pairs (hash-ref declared-enums enum-name)]
            [pair (memf (λ (p) (equal? (car p) key)) pairs)])
           (cdr (car pair))))))

  (define (add-enum enum-name vals)
    (printf "enum ~a\n" (symbol->string enum-name) )
    (for ([kvp vals])
      (printf "~a : ~x\n" (car kvp) (cdr kvp)))
    (hash-set! declared-enums (symbol->string enum-name) vals))

  (define-syntax-class enum
    #:description "a declared enum"
    #:opaque
    (pattern x:id #:when (enum-exists? (attribute x) (symbol->string (syntax-e (attribute x))))))

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
             #:cut
             #:fail-unless (enum-exists?  (attribute x) (car split))
             (format "the enum ~a does not exist" (car split))
             #:fail-unless (enum-key-exists? (attribute x) (car split) (car (cdr split)))
             (format "the value ~a does not exist for enum ~a"
                     (car (cdr split))
                     (car split))
             #:with value (datum->syntax this-syntax (get-enum-value (attribute x) (car split) (car (cdr split))))
             #:with compiled
             (datum->syntax this-syntax
                            (format "~a (~a)" (symbol->string (syntax-e (attribute x)))
                                    (get-enum-value (attribute x) (car split) (car (cdr split)))))
             #:with bits (datum->syntax this-syntax (string-length (format "~b" (get-enum-value (attribute x)(car split) (car (cdr split))))))
             ))

  ;important note: these mutable structs do not work "globally", they are for
  ;local expansion purposes only. the modules and ports are also exposed via
  ;static bindings for other files to see. 
  (struct port-meta (name direction type) #:transparent)
  (struct func-meta (name size-int) #:transparent #:mutable)
  (struct module-meta (name ports functions) #:transparent #:mutable)
  (define module-metadata (make-hash))
  (define (add-module name ports)
    (printf "adding module ~a\n" name)
    (printf "~a\n" (hash-keys module-metadata))
    (if (hash-has-key? module-metadata name)
        (error "module ~a already exists" name)
        (hash-set! module-metadata name (module-meta name ports '()))))
  (define (module-exists? name-stx)
    ;here we check for a static bidning to this works across files.
    ;local metadata only exists for local function definitions
    (syntax-local-value name-stx (λ () #f)))

  (define (module-has-port? module-name port-name)
    ;todo: rewrite this to use static binding data.
    (memf (λ (port) (equal? (port-meta-name port) port-name))
          (module-meta-ports (hash-ref module-metadata module-name))))
  (define (module-has-function? module-name function-name)
    (memf (λ (func) (equal? (func-meta-name func) function-name))
          (module-meta-functions (hash-ref module-metadata module-name))))
  (define (add-module-function module-name function-name size)
    (let* ([mod (hash-ref module-metadata module-name)]
           [fs (module-meta-functions mod)])
      (set-module-meta-functions! mod (cons (func-meta function-name size) fs))))

  
  (define-syntax-class module-param
    #:description "a module initializer"
    (pattern [port-name:id port-value:bound-usage] 
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'port-name)))       
             #:with value(datum->syntax this-syntax #'port-value.compiled))

    (pattern [port-name:id port-value:expr] 
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'port-name)))       
             #:with value(datum->syntax this-syntax #'(expression port-value))))
  
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

  (struct binding-meta ( stx-size stx-arity-list))
  (define (add-scoped-binding stx-name binding-meta stx)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (when (and (in-scope? name) (not (equal? name "global")))
        (writeln
         (format "warning: ~a is already in scope at ~a"
                 name (source-location->string stx))))
      (hash-set! scoped name binding-meta)))

  (define (remove-scoped-binding stx-name)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (hash-remove! scoped name)))

  (define (in-scope? name)
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
           (binding-meta-stx-size (hash-ref (car lst) name2)))]
        [else (aux (cdr lst))]))
      (aux (unbox scoped-bindings-stack))))

  (define (get-binding-arities name)
    (let ([name2 (if (syntax? name) (symbol->string (syntax-e name)) name)])
      (define (aux lst)
      (cond
        [(empty? lst)
         (begin
           'none)]
        [(hash-has-key? (car lst) name2)
         (begin
           (binding-meta-stx-arity-list (hash-ref (car lst) name2)))]
        [else (aux (cdr lst))]))
      (aux (unbox scoped-bindings-stack))))

  (define-syntax-class scoped-binding
    #:description "identifier in scope"
    #:commit
    (pattern x:id
             #:with name  (symbol->string (syntax-e #'x))
             #:with name-stx (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
             #:fail-unless (in-scope? (symbol->string (syntax-e #'x))) "identifier is not in scope."
             #:with size-int (get-binding-size (symbol->string (syntax-e #'x)))
             #:with arities (get-binding-arities (symbol->string (syntax-e #'x)))
             #:with is-array?
             (let* ([a (get-binding-arities (symbol->string (syntax-e #'x)))]
                    [b (if (syntax? a)(list?(syntax-e a)) #f)] )
             (and (syntax? a) (list? (syntax-e a)))
             )))

  (define-syntax-class binding
    #:description "identifier name"
    (pattern x:id
             #:with name (symbol->string (syntax-e #'x))))

  (define-syntax-class scoped-function
    (pattern x:id
             #:with name (symbol->string (syntax-e #'x))
             #:with name-stx (datum->syntax this-syntax (symbol->string (syntax-e #'x)))
             #:when (module-has-function? current-module (symbol->string (syntax-e #'x)))
             )
    )
             
  (define-syntax-class inner-usage
    (pattern x:scoped-binding
             #:with name #'x.name
             #:with size-int #'x.size-int
             #:with compiled
             #'x.name-stx)
    (pattern x:expr
             #:with size-int #'(expression x)
             #:with compiled #'(expression x)))
    
  (define-syntax-class bound-usage
    #:description "identifier in scope with or without size, or array access"
    #:commit

    ;arrays:
    ;when accessing an array, verilog says you must use all the dimensions.
    ;following that, you can further index into the bits using the normal
    ;range syntax.

    ;to start with no range checking of arrays. but we must still know
    ;the length of the array to know if they have supplied a range at the
    ;end or not (up to two expressions)
    (pattern [s:scoped-binding
              x:inner-usage ...+]
             #:with x-count (length (syntax->list #'(x ...)))
             #:with name #'s.name
             
             #:with oob #'#f ;todo; out of bounds checks
             #:with compiled
             ;todo: report these errors properly, not using exceptions!!
             ;todo: range checking on arities.
             (if (syntax-e #'s.is-array?)
                 (cond
                   [(< (syntax-e #'x-count) (length (syntax-e #'s.arities)))
                    (error "you must specify all the array's dimensions" #'s)]
                   [(= (syntax-e #'x-count) (length (syntax-e #'s.arities)))
                    #'`(name  ("[" ,x.compiled  "]") ...)]
                   [else
                    (let-values
                        ([(left right)
                          (split-at
                           (syntax->list #'(x ...))
                           (length (syntax-e #'s.arities)))])
                      (syntax-parse (list left right)
                        [((z:inner-usage ...) (ya:inner-usage yb:inner-usage))
                         #'`(name ("[" z.compiled  "]") ...
                                   "[" ya.compiled " : " yb.compiled "]"
                                   )]
                        [((z:inner-usage ...) (ya:inner-usage))
                         #'`(name ("[" z.compiled  "]") ...
                                   "[" ya.compiled "]"
                                   )]
                        [((z:inner-usage ...) ())
                         #'`(name ("[" z.compiled  "]") ...)]))])
                 (cond
                   [(> (syntax-e #'x-count) 2) (error "not an array\n" #'s)]
                   [(= (syntax-e #'x-count) 2) 
                    (syntax-parse #'(x ...)
                      [(x:inner-usage y:inner-usage)
                       #'`(name "[" ,x.compiled " : " ,y.compiled "]")])]
                   [else
                    #'`(name ("[" ,x.compiled  "]") ...)]))
                   
             #:with name-stx #'compiled

             #:with size-int 
             ;since it is not possible to compile an array expression without
             ;all the indexes, we need only return the atual data size
             ;OR whatever the range equates to.  for non-arrays, the size will
             ;be either one for a signle bit select or the size of the range.

             (if (syntax-e #'s.is-array?)
                 (let-values
                     ([(left right)
                       (split-at (syntax->list #'(x ...))
                                 (length (syntax-e #'s.arities)))])
                      (syntax-parse (list left right)
                        [((z:inner-usage ...) (msb:inner-usage lsb:inner-usage))
                         #'(+ (- msb.size-int lsb.size-int) 1)]
                        [((z:inner-usage ...) (ya:inner-usage))
                         ;single bit
                         #'1]
                        [((z:inner-usage ...) ())
                         ;indexed - return size of array data                         
                         #'s.size-int]))                 
                 (syntax-parse #'(x ...)
                   [(msb:inner-usage lsb:inner-usage)
;                    (printf "here size is ~a ~a \n" #'msb.size-int #'lsb.size-int
                           ;)
                    #'(+ (- msb.size-int lsb.size-int) 1)]
                   [(x:inner-usage)
                    #'1])
                   
                 ))

        (pattern s:scoped-binding
             #:with name #'s.name
             #:with size (datum->syntax this-syntax "")
             #:with size-int #'s.size-int
             #:with oob #'#f
             #:with compiled   (datum->syntax this-syntax (symbol->string (syntax-e (attribute s))))
             #:with name-stx #'compiled) ;used in error reporting

    ))

(define-syntax (push-binding stx)
  (syntax-parse stx
    [(_ id size)
     (add-scoped-binding #'id (binding-meta #'size #'#f) stx)
     #'(void)]
        [(_ id size arity-list)
     (add-scoped-binding #'id (binding-meta #'size #'arity-list) stx)
     #'(void)]))

(define-syntax (pop-scoped-stack stx)
  (syntax-parse stx
    [(_)
     (pop-scoped-stack)
  #'(void)]))

(define-syntax (toggle-always-sens stx)
  (syntax-parse stx
    [(_)
     (toggle-always-sens)
  #'(void)]))

(begin-for-syntax
  (define (syntax->error-syntax stx)
    (datum->syntax stx
    (format "~a:~a:~a"
            (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx))))

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
                           ;todo: this needs work, probably want tot just parse and count binary  instead?
                           (case radix
                             [(2) (string-length n)]
                             [(8) (* (string-length n) 3)]
                             [(16) (string-length (format "~b" (string->number n 16))
)]
                             [(10) (string-length (format "~b" (string->number n 10))
)])])
                     (when (> l size)
                       (printf "warning: number literal ~a does not fit into the specified size at ~a\\n"
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
    #:no-delimit-cut
    (pattern [edge:edge-type ~! signal:bound-usage]
             #:with edge-type (datum->syntax this-syntax (keyword->string (syntax-e #'edge)))
             #:with compiled #'signal.compiled)
    (pattern [signal:bound-usage]             
             #:with edge-type (datum->syntax this-syntax "")
             #:with compiled #'signal.compiled)
    )

  (define-syntax-class direction-option
    (pattern #:input)
    (pattern #:output)
    (pattern #:inout))

  (define-syntax-class type-option
    (pattern #:wire)
    (pattern #:wand)
    (pattern #:wor)
    (pattern #:tri)
    (pattern #:reg)
    (pattern #:integer)
    (pattern #:time)
    (pattern #:real))

  (define-syntax-class function-param
    #:description "a function parameter"
    (pattern [name-sym:id
              (~optional [x (~optional y)])]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with direction (datum->syntax this-syntax "input")
             #:with type (datum->syntax this-syntax "wire")
             #:with arity-list #'#f
             #:with default #'""
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

    (define-syntax-class param
    #:description "a module parameter"
    (pattern [name-sym:id
              direction-opt:direction-option
              type-opt:type-option
              (~optional [x (~optional y)])
              (~optional default-value)]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with direction (datum->syntax this-syntax (keyword->string (syntax-e #'direction-opt)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-opt)))
             #:with default
             (if (attribute default-value)
                 #'`(" = " ,(expression default-value))
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
    #:datum-literals (array)

   (pattern [name-sym:id
              type-opt:type-option
              [x (~optional y)]
              (~optional (array x2:expr ...+))]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-opt)))
             #:with default ;arrays dont have defaults, instead the
                            ;additional array syntax appears here.
             (cond
               [(and (attribute x2))
                #'`(
                    (
                     "[0:" ,(- x2 1) "]"
                    ) ...
                    )]
               [else #'""])                     
             
             #:with arity-list
             (if (attribute x2)
                 (syntax->list #'(x2 ...))
                 #'#f)
             
             ; actual data size, not array dimensions.
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
              type-opt:type-option
              (~optional [x (~optional y)])
              (~optional
               default-value:expr)]
             #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
             #:with type (datum->syntax this-syntax (keyword->string (syntax-e #'type-opt)))
             #:with default
             (if (attribute default-value)
                 #'`(" = " ,(expression default-value))
                 #'"")
             #:with arity-list #'#f
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
               [else #'""]))))
         

(define-syntax-parser expression
  #:datum-literals
  (set ~delay if case else when concat
   \|\| \| \~\| ! ~ + - * / % << >> >>> == != >= <= < > && & ~&  ^ ~^ )
  [(_ x:integer)
   #'x]
  [(_ x:number-literal )
   #'x.compiled]
  [(_ x:bound-usage)
   #:with err-prefix (syntax->error-syntax #'x)
   #'`(
       ,(when x.oob
         (printf "~a: warning - the expression '~a' is out of range\n" err-prefix x.compiled))
       ,x.compiled)]
  [(_ x:enum-literal)
   #'x.value]
  [(_ (f:scoped-function ~! params ... last-param))
   #'`(
       ,f.name-stx "("
       ( ,(expression params ) ",") ...
       ,(expression last-param)
       ")")]       
  [(_ (~delay ~! x y))
   #'`("#" ,(expression x) " " ,(expression y))]
  [(_ (when ~! test true-expr))
   ;special case one-line when in RHS of expression - ternary
   #'(~begin (when test true-expr))]
  [(_ (concat ~! x y ...+))
   #'`("{" ,(expression x)  ( ", ",(expression y)) ... "}" )]
  [(_ (if  ~!
           (~describe "condional test for if" test)
           (~describe "true expression for if" true-expr)
           (~describe "false expression for if" false-expr)))
   #'`("("
       ,(expression test)
       " ? "
       ,(expression true-expr)
       " : "
       ,(expression false-expr)
       ")")]
  [(_ (case val
             [test true-expr]
             [test2 expr2] ...+
             [else def-expr]))
   #'`(
       "("
       ,(expression (== val test))
       " ? "
       ,(expression true-expr)
       " : "
       ,(expression (case val [test2 expr2] ... [else def-expr]))
       ")")]
  [(_ (case val [test true-expr]
                 [else def-expr]))
   #'`(
       "("
       ,(expression (== val test))
       " ? "
       ,(expression true-expr)
       " : "
       ,(expression def-expr)
       ")")]
  [(_ (case ~! val [test true-expr] ...+))
   #:fail-when #t "you must supply an else branch of a case when used as an epxression"
   #'(void)]

  ; unary

  [(_ ( (~and op (~or + - ! & ~& ~ \| \~\| ^ ~^)) x))
   #:with op-str (datum->syntax this-syntax (symbol->string (syntax-e #'op)))
   #'`(,op-str  ,(expression x))]

  ; binary  
  [(_ ( (~and op (~or + - * / % << >> >>> == != < > <= >= && & \|\| \| ^ ~^)) x y ))
   #:with op-str (datum->syntax this-syntax (symbol->string (syntax-e #'op)))
   #'`(
       "("
       ,(expression x)
       " "
       ,op-str
       " "
       ,(expression y)
       ")")]
  [(_ ( (~and op (~or + - * / % << >> >>> == != <= >= && & \|\| \| ^ ~^)) x y z ... ))
   #:with op-str (datum->syntax this-syntax (symbol->string (syntax-e #'op)))
      #'`(
          "("
          ,(expression x)
          " "
          ,op-str
          " ("
          ,(expression (op y z ...))
          ")) " )]

  ;setters and bounds / truncation checking
  [(_ (set (~or x:scoped-binding x:bound-usage) y:number-literal))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(when (> y.bits x.size-int)
          (printf "\"warning: the literal '~a' does not fit into '~a' and will be truncated\"\n" y.compiled x.name-stx))       
       ,(expression x)
       op
       ,(expression y))]

  [(_ (set (~or x:scoped-binding x:bound-usage) y:enum-literal))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(when (> y.bits x.size-int)
          (printf "\"warning: the enum literal '~a' does not fit into '~a' and will be truncated\"\n" y.compiled x.name-stx))       
       ,(expression x)
       op
       ,(expression y))]

  [(_ (set (~or x:scoped-binding x:bound-usage) (~or y:scoped-binding y:bound-usage)))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(when (> y.size-int x.size-int)
          (printf "\"warning: the expression '~a' does not fit into '~a' and will be truncated\"\n" y.name-stx x.name-stx))       
       ,(expression x)
       op
       ,(expression y))]

  [(_ (set (~or x:scoped-binding x:bound-usage) y:expr))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #:with name (datum->syntax this-syntax (format "~a" #'y))
   #'`(
       ,(when (and (number? (expression y))(> (string-length (format "~b" (expression y))) x.size-int))
          (printf "\"warning: the expression '~a' does not fit into '~a' and will be truncated\"\n" name  x.name-stx))       
       ,(expression x)
       op
       ,(expression y))]
    
  [(_ (set x y))
   #:with op (if is-always-sens #'" <= " #'" = ")
   #'`(
       ,(expression x)
       op
       ,(expression y))]

  [(_ x:expr)
   #'x]
  
  )

   
(define-syntax-parser ~case
  #:datum-literals (else)
  [(_ test:bound-usage [lhs:number-literal rhs (~optional comment:string #:defaults ([comment #'""]))] ...)
   #'`(
       tab
       "case ("
       ,test.compiled
       ")\n"
       inc-tab
       (
        tab
        ,lhs.compiled
        " : // "
        comment
        "\n"
        ,(~begin rhs)
        "\n"
        ) ...
       dec-tab
       tab
       "endcase\n")]
  [(_ test:bound-usage [lhs:number-literal rhs (~optional comment:string #:defaults ([comment #'""]))] ...
      [else else-expr:expr])
   #'`(
       tab
       "case ("
       ,test.compiled
       ")\n"
       inc-tab
       (
        tab
        ,lhs.compiled
        " : // "
        comment
        "\n"
        ,(~begin rhs)
        "\n"
        ) ...

       tab
       "default : \n"
       ,(~begin else-expr)
       "\n"
       dec-tab
       tab
       "endcase\n")]
)

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
       ,(expression first-test)       
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
       ,(expression first-test)       
       ")\n"
       inc-tab       
       ,(~begin first-outcome)
       "\n"
       dec-tab
       (tab
        "else if("
        ,(expression expr-test)
        ")\n"
        inc-tab
        ,(~begin expr-outcome)
        "\n"
        dec-tab
        "\n") ...                
         )])

(define-syntax-parser ~if
  [(_ (~describe "condional test for if" test-expr)
      (~describe "true expression for if" true-expr)
      (~describe "false expression for if" false-expr))
   #'(~cond
      [test-expr true-expr]
      [else false-expr])])

(define-syntax-parser ~when
  [(_ test-expr true-expr)
   #'(~cond
      [test-expr true-expr])])

(define-syntax-parser list->enum
  [(_ name vals)
   (add-enum (syntax-e #'name) (eval #'vals))
   #'(void)])

;; (define-syntax (new-enum stx)
;;   (printf "JKJK")
;;   (if (syntax-property stx 'module)
;;       (with-syntax ([n (symbol->string (syntax-e (syntax-property stx 'module)))])
;;       #'(printf "true  ~a \n" n))
;;       #'(print "false\n")))
  

(define-syntax-parser enum?
  [(_ name)
   (printf "enum testing ~a\n" #'name)
   (printf "enum testing ~a\n" #'global-enum-test-enum)
   (if (syntax-local-value (datum->syntax #'name 'global-enum-test-enum) (λ () #f))
       (with-syntax ([vals (syntax-local-value #'global-enum-test-enum (λ () #f))])
         (begin (printf "enum exists\n")
                #'(void)))
        (begin (printf "enum not exists\n")
               #'(void)))])


(define-syntax-parser enum
  [(_ name kvp:enum-kvp ...+)
   #:fail-when (check-duplicate-identifier
                (syntax->list #'(kvp.x ...)))
    "duplicate enum name"
   #:fail-when (check-duplicates
                (syntax->datum #'(kvp.y-evaled ...)))
   "duplicate enum value"

   (if (syntax-property this-syntax 'module)
       (begin
         ;a local enum only need exist for this module during this expansion
         (add-enum (syntax-e #'name) (syntax->datum #'(kvp.pair ...)))
         #'(void))
       ;otherwise we create a static binding for the enum data
       ;prefixing the name with global-enum
       (with-syntax ([g-name (datum->syntax this-syntax (string->symbol
                                                         (string-append "global-enum-"
                                                                      (symbol->string
                                                                       (syntax-e #'name)))))])
         (printf "ADDING ENUM ~a\n" #'g-name)
         #'(define-syntax g-name             
             '(kvp.pair ...)
             )))]
  [(_ name keys:id ...+)
   #:fail-when (check-duplicate-identifier
                (syntax->list #'(keys ...)))
   "duplicate enum name"
   (with-syntax
     ([(kvps ...)
       (for/list
           ([n (in-naturals)]
            [x (syntax->list #'(keys ...))])
         (cons (format "~a" (syntax-e x)) n))])
   (if (syntax-property this-syntax 'module)
       (begin
         (add-enum (syntax-e #'name)(syntax->datum #'(kvps ...)))
         #'(void))
       (with-syntax ([g-name (datum->syntax this-syntax (string->symbol
                                                         (string-append "global-enum-"
                                                                        (symbol->string
                                                                         (syntax-e #'name)))))])
         #'(define-syntax g-name             
             '(kvps ...)
             ))
       )
       )])

(define-syntax-parser ~match-set
  [(_ target:bound-usage test:expr enum-name:enum
      [key value] ...)
   #:fail-when (check-duplicate-identifier (syntax->list #'(key ...)))
    "duplicate enum value"

   #:fail-when
   (let ([results (filter (λ (v) (not (enum-key-exists? #'enum-name (syntax-e #'enum-name) v)))
                          (syntax->datum #'(key ...)))])
     (if (not (eq? results '()))
         (with-syntax ([res results])  #'res)
       #f))
   "some identifiers do not exist in enum"

   #:fail-when
   (let*
       ([keys (map (λ (v) (format "~a" v)) (syntax->datum #'(key ...)))]
        [results (filter (λ (v) (not (member v keys)))
                         (get-enum-keys #'enum-name (syntax-e #'enum-name)))])
     (if (not (eq? results '()))
         (with-syntax ([res results]) #'res)
         #f))
   "missing cases in the enum"
     
   (with-syntax([(enum-vals ...) (map (λ (v) (get-enum-value #'enum-name (syntax-e #'enum-name) v))
                                      (syntax->datum #'(key ...)))])
     #'(~case test [enum-vals (set target value)] ...))]
  )

(define-syntax-parser ~match
  [(_ test:expr enum-name:enum
      [key expr] ...)
   #:fail-when (check-duplicate-identifier (syntax->list #'(key ...)))
    "duplicate enum value"

   #:fail-when
   (let ([results (filter (λ (v) (not (enum-key-exists? #'enum-name (syntax-e #'enum-name) v)))
                          (syntax->datum #'(key ...)))])
     (if (not (eq? results '()))
         (with-syntax ([res results])  #'res)
       #f))
   "some identifiers do not exist in enum"

   #:fail-when
   (let*
       ([keys (map (λ (v) (format "~a" v)) (syntax->datum #'(key ...)))]
        [results (filter (λ (v) (not (member v keys)))
                         (get-enum-keys #'enum-name (syntax-e #'enum-name)))])
     (if (not (eq? results '()))
         (with-syntax ([res results]) #'res)
         #f))
   "missing cases in the enum"
     
   (with-syntax
     ([(enum-vals ...) (map (λ (v) (get-enum-value #'enum-name (syntax-e #'enum-name) v))
                            (syntax->datum #'(key ...)))]
      [(key-str ...) (map (λ (v) (symbol->string v))
                          (syntax->datum #'(key ...)))] )
     #'(~case test [enum-vals expr key-str] ...))]
)

(define-syntax-parser ~case-set
  [(_ target:bound-usage test:expr
      [key:number-literal value] ...)     
     #'(~case test [key (set target value)] ...)])

(define-syntax-parser ~begin-line
  #:datum-literals (~cond locals expression ~when if set ~match-set ~match ~case-set)  
  [(_ (expression expr ...))
   #'`(tab
       ,(expression expr ...)
       ";\n")]  
  [(_ (set [x:bound-usage y] ...))
   #'`(
       (tab
        ,(expression (set x y))
        ";\n")...)]
  [(_ (set x:bound-usage y))
   #'`(
       (tab
        ,(expression (set x y))
        ";\n"))]
  [(_ x:expr)
   #'x]   )

(define-syntax-parser ~inc
  [( _ x:scoped-binding)
   #'`(tab
       ,(expression (set x (+ x 1)))
       ";\n"
       )])

(define-syntax-parser ~begin
  [(_ block-name:id expr ...+)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'block-name)))
   #'`(
       tab
       "begin "
       ,name
       "\n"
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       "end \n"
       )]
  [(_  expr ...)
   #'`(
       tab
       "begin\n"
       inc-tab
       ,(~begin-line expr) ...
       dec-tab
       tab
       "end \n"
       )])

(define-syntax-parser locals
  [(_ params:local-param ...)
   #'`(
       (
        tab
        ,(push-binding params.name params.size-int params.arity-list) ...
        (
         ,params.type
         " "
         ,params.size
         " "
         ,params.name
         " "
         ,params.default
         ";\n") ...))])

(define-syntax-parser assign 
  [(_ [x:bound-usage y:expr] ...)
   #'`(
       ("assign "
       ,x.compiled
       " = "
       ,(expression y)
       ";\n") ...  )]
  [(_ x:bound-usage y:expr)
   #'`("assign "
       ,x.compiled
       " = "
       ,(expression y)
       ";\n")])

(define-syntax-parser always-line  
  [(_ expr)
   #'expr])

(define-syntax-parser always
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
         ,rest.compiled
       ) ...
       ")\n"
       inc-tab
      ,(always-line expr) ...
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
         ,rest.compiled) ...
       ")\n"
       inc-tab
       ,(always-line expr) ...
       dec-tab
       ,(toggle-always-sens)
       )]
  [(_ * expr ...)
   (toggle-always-sens)
   #'`(
       tab
       "always @(*)\n"
       inc-tab
       ,(always-line expr) ...
       dec-tab
       ,(toggle-always-sens)
       )]
    [(_ expr ...)
   #'`(
       tab
       "always\n"
       inc-tab
       ,(always-line expr) ...
       dec-tab
       )]
    )

(define-syntax-parser ~module-line
  #:datum-literals (set vmod) 
  ;; [(_ mod-id (set [x:bound-usage y] ...))
  ;;  (syntax-property
  ;;  #'`((tab
  ;;       ,(expression (set x y))
  ;;       "a;\n") ...)
  ;;  'module
  ;;  #'mod-id)
  ;;  ]
  ;; [(_ mod-id (set x:bound-usage y))
  ;;  (syntax-property
  ;;   #'`(tab
  ;;       ,(expression (set x y))
  ;;       "b;\n")
  ;;   'module
  ;;   #'mod-id)
  ;;   ]
  [(_ mod-id (vmod m:id  ~! p:module-param ... l:module-param ~!))

   #:fail-unless (module-exists? #'m)
   (format "the module '~a' doesn't exist"(symbol->string (syntax-e #'m)))

;   #:fail-unless
   ;; (andmap (λ (lst) (module-has-port?  (symbol->string (syntax-e #'m)) lst))
   ;;         (syntax->datum #'(p.name ... l.name)))

   ;; ;; ;todo: show which fields are missing
   ;;  "module instantation does not contain all module fields"

         (printf "in mod init\n")
   (with-syntax([m-name (symbol->string (syntax-e #'m))]
                [i-name (symbol->string (syntax-e #'x))])
     (syntax-property
     #'`(
       ,m-name
       " (\n"
       inc-tab
       (
        "." ,p.name "(" ,(expression p.value) "),\n" 
       ) ...

       "." ,l.name "(" ,(expression l.value) ")\n" 
       dec-tab
       ");\n"

       )
     'module
     #'mod-id))
     ]
  [(_ mod-id x)
   (syntax-property #'x 'module #'mod-id)])

(define-syntax-parser function
  [(_ (~optional [x (~optional y)])
      name-sym:id
       ; output size
      (p:function-param ...) 
      expression ...)
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
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
     [else #'""])
   (push-scoped-stack)
   (add-module-function current-module (symbol->string (syntax-e #'name-sym))
                        (syntax-e #'size-int))
   #'`(
       "function " ,size " " ,name ";\n"
       inc-tab
       tab
       ;push the name and size of the function as it is used
       ;to set the return value. sticking to Verilog style for now.
       ,(push-binding name size-int #f)
       ,(push-binding p.name p.size-int p.arity-list) ...
       (tab
        ,p.direction
        " "
        ,p.size
        " "
        ,p.name
        ";\n") ...       
       ,(~begin
          expression ...)
       dec-tab
       ,(pop-scoped-stack)
       "endfunction\n")])

(define-syntax-parser testaa
  [(_ name-sym:id)
   (if (syntax-local-value #'name-sym (λ () #f))
       (with-syntax ([x (syntax-local-value #'name-sym)])
         #''x)
       #'(error "fail"))])


(define out-port #f)
(define (ensure-port-open filename)
  (when (or (not (port? out-port))(port-closed? out-port))
    (printf "opening file ~a for write ...\n" filename)
    (set! out-port (open-output-file #:mode 'binary #:exists 'replace filename))))
(define (ensure-port-closed)
  (when (and (port? out-port) (not (port-closed? out-port)))
    (close-output-port out-port)))


(define-syntax-parser #%module-begin
  [(_ exprs ...)
   #'(#%plain-module-begin
      (printf "MOD BEGIN\n")

      exprs ...
      (ensure-port-closed))])
     


(define-syntax-parser vmod
  [(_ name-sym:id
      (p:param ... last:param) ;inputs
      expression ... )
   #:with name (datum->syntax this-syntax (symbol->string (syntax-e #'name-sym)))
   (push-scoped-stack)
   (set-current-module (symbol->string (syntax-e #'name-sym)))
   (add-module (syntax-e #'name)
               '()
               ;; (map (λ (lst)
               ;;        (port-meta
               ;;         (list-ref lst 0)
               ;;         (list-ref lst 1)
               ;;         (list-ref lst 2)                 
               ;;         ))
               ;;      (syntax->datum #'(p ... last)))
               )
   (let*
       ([fn (string-replace (path->string (syntax-source-file-name this-syntax)) ".rkt" ".v")])
        
     (with-syntax
       ([nf (datum->syntax this-syntax (build-path (syntax-source-directory this-syntax) fn))]
      

         )
       (syntax-property       
       #'(begin
              (ensure-port-open nf)
       (define-syntax name-sym
         (list
         (map (λ (lst)
                      (port-meta
                       (list-ref lst 0)
                       (list-ref lst 1)
                       (list-ref lst 2)                 
                       ))


              (syntax->datum #'(p ... last)))))
     (provide name-sym)
     (code-gen-2
        `(
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

         ,(~module-line name-sym expression) ...
         
          "endmodule\n"
          ,(pop-scoped-stack)
         )))
       'module
       #'name-sym
       )

       ))])

  

(define-syntax-parser always-pos
  [(_ clock exprs ...)
   #'(always ([#:posedge clock]) (~begin exprs ...))])

(define-syntax-parser always-neg
  [(_ clock exprs ...)
   #'(always ([#:negedge clock]) (~begin exprs ...))])

(define-syntax-parser initial-begin
  [(_ exprs ...) #'`("initial " ,(~begin exprs ...))])

(define-syntax-parser vfile
  [(_ filename exprs ...)
   #'(code-gen
      (list exprs ...)
      filename)])

(define (code-gen-2 input )
;  (printf "codegen2 with ~a \n" input)
  (define tab 0)
  (define (aux in)
    (for ([sym in])
;      (printf "~a\n" sym)
      (cond
        [(or (string? sym) (integer? sym))
         (begin
           (display sym out-port))]
        [(eq? 'inc-tab sym) (set! tab (+ 1 tab))]
        [(eq? 'tab sym)     (display (make-string (* 2 tab) #\ ) out-port)]
        [(eq? 'dec-tab sym) (set! tab (- tab 1))]
        [(eq? '() sym) '()]
        [(list? sym) (aux sym)]
        [(void? sym) '()]
        [else (printf "unknown ~a\n" sym)
              ])))
  (printf "writing to port ~a ... \n" out-port)
  (aux input)
  (printf "finished.\n"))

(define (code-gen input filename)
  (define tab 0)
  (define out (open-output-file #:mode 'binary #:exists 'replace filename))
  (define (aux in)
    (for ([sym in])
;      (printf "~a\n" sym)
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
        [else (printf "unknown ~a\n" sym)
              ])))
  (printf "generating file ~a ... \n" filename)
  (aux input)
  (close-output-port out)
    (printf "finished.\n"))

(provide

 (all-defined-out)
 (for-syntax  (all-defined-out))
 (except-out (all-from-out syntax/parse/define)
             define-syntax-parser)
 (rename-out
  [define-syntax-parser macro]))

