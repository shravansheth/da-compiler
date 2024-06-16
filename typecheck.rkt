#lang typed/racket
(require typed/json)
(require typed/rackunit)

;; global variables 

;;-----------------------------------AST STRUCTS SETUP------------------------------------------------
;; top level program struct
;; types is list of all the structs defined at the top
;; declarations is a list of global variables defined
;; functions is the list of functions defined


(struct Program ([types : (Listof ty-Struct)]
                 [declarations : (Listof Declaration)]
                 [functions : (Listof Func)]) #:transparent)

(define-type Types-list (Listof ty-Struct))
(define-type Decls-list (Listof Declaration))
(define-type Funcs-list (Listof Func))

;; Function type
(struct Func ([id : Identifier]
              [parameters : Decls-list]
              [ret-type : Type]
              [declarations : Decls-list]
              [statements : (Listof Statement)]) #:transparent)

;; Types struct
(define-type Type (U ty-Int ty-Bool ty-Struct ty-Array ty-Void))
(struct ty-Int () #:transparent)
(struct ty-Bool () #:transparent)
(struct ty-Struct ([id : Identifier]
                   [fields : (Listof NestedDeclaration)]) #:transparent)
(struct ty-Array () #:transparent)
(struct ty-Void () #:transparent)

(struct Field ([type : Type] [id : Identifier]) #:transparent)

;; Nested declarations
(define-type NestedDeclaration (U NestedField NestedStructType NestedArrayType))
(struct NestedField ([type : Type]
                     [id : Identifier]) #:transparent)
(struct NestedStructType ([id : Identifier]
                          [fields : (Listof NestedDeclaration)]) #:transparent)
(struct NestedArrayType ([type : Type]) #:transparent)



;; Literal structs
(define-type Literal (U NumLit
                        BoolLit
                        NullLit
                        NewExpr
                        NewArray
                        VoidLit
                        ArrLit))
(struct NumLit ([value : Integer]) #:transparent)
(struct BoolLit ([value : Boolean]) #:transparent)
(struct NullLit () #:transparent)
(struct NewExpr ([type-id : Identifier]) #:transparent)
(struct NewArray ([size : Natural]) #:transparent)
(struct VoidLit () #:transparent)
(struct ArrLit ([len : Natural]) #:transparent)

(struct Identifier ([name : String]) #:transparent) 

;; Decl-single struct (Used in parameters)
(struct Decl-single ([type : Type]
                     [id : Identifier]) #:transparent)

;; Declaration struct (Used in Program declarations- GLOBAL VARIABLE)
(struct Declaration ([type : Type]
                     [id : Identifier]) #:transparent)

;; Expression struct
(define-type Expression (U BoolExpr
                           EqExpr
                           RelExpr
                           SimpleExpr
                           TermExpr
                           UnaryExpr
                           SelectorExpr
                           Identifier
                           Literal
                           Invocation
                           Read
                           IndexExpr))

;; Binary expressions: boolean, equality, relational, simple, term
(struct BoolExpr ([left : Expression]
                  [operator : String]
                  [right : Expression]) #:transparent)
(struct EqExpr
  ([left : Expression]
   [operator : String]
   [right : Expression]) #:transparent)

(struct RelExpr
  ([left : Expression]
   [operator : String]
   [right : Expression]) #:transparent)

(struct SimpleExpr
  ([left : Expression]
   [operator : String]
   [right : Expression]) #:transparent)

(struct TermExpr
  ([left : Expression]
   [operator : String]
   [right : Expression]) #:transparent) 

;; Unary expressions and selector expressions
(struct UnaryExpr ([operator : String]
                   [expr : Expression]) #:transparent)

(struct SelectorExpr ([expr : Expression]
                      [selector : Identifier]) #:transparent)

(struct IndexExpr ([left : Expression] [index : Expression]) #:transparent)


;; Read struct
(struct Read () #:transparent)


;; Statement struct
(define-type Statement (U
                        Block
                        Assignment
                        Print
                        Conditional
                        Loop
                        Delete
                        Ret
                        Invocation))
(struct Block ([stmts : (Listof Statement)]) #:transparent)
(struct Assignment ([target : Expression]
                    [source : Expression]) #:transparent)
(struct Print ([expr : Expression]
               [endl : Boolean]) #:transparent)
(struct Conditional ([guard : Expression]
                     [then : Block]
                     [else : Block]) #:transparent)
(struct Loop ([while-cond : Expression]
              [then-do : Block]) #:transparent)
(struct Delete ([del : Expression]) #:transparent)
(struct Ret ([ret : Expression]) #:transparent)
;; that's a function call
(struct Invocation ([id : Identifier]
                    [args : (Listof Expression)]) #:transparent)

;;-----------------------------------X------------------------------------------------





;;--------------------------------Make Types AST--------------------------------------
;; function to parse types from the list of hashtables
(define (make-types [types : (Listof (HashTable Any Any))]) : Types-list
  (match types
    ['() '()]
    [(cons f r)
     (define struct-name (cast (hash-ref f 'id) String))
     (define parsed-struct-fields (parse-fields (cast (hash-ref f 'fields) (Listof (HashTable Any Any))))) 
     (append (list (ty-Struct (Identifier struct-name) parsed-struct-fields))
             (make-types r))
     ])) 

;; function to parse the fields of a struct definition
(define (parse-fields [fields : (Listof (HashTable Any Any))]) : (Listof NestedDeclaration)
  (match fields
    ['() '()]
    [(cons f r)
     (define field-name (cast (hash-ref f 'id) String))
     (define field-type (hash-ref f 'type))
     (match field-type
       ["int" (append (list (NestedField (ty-Int) (Identifier field-name))) (parse-fields r))]
       ["bool" (append (list (NestedField (ty-Bool) (Identifier field-name))) (parse-fields r))]
       ["int_array" (append (list (NestedField (ty-Array) (Identifier field-name))) (parse-fields r))]
       [else (append (list (NestedField (ty-Struct (Identifier (cast field-type String)) '())
                                        (Identifier field-name)))
                     (parse-fields r))])
     ]))
;;------------------------------------XXX-------------------------------------------------





;;--------------------------------Make Declarations AST-----------------------------------
;; Function to setup declarations(global variables) in AST form
(define (make-declarations [decls : (Listof (HashTable Any Any))]) : (Listof Declaration)
  (match decls
    ['() '()]
    [(cons f r)
     (define dec-name (cast (hash-ref f 'id) String)) 
     (define dec-type (hash-ref f 'type))
     (match dec-type
       ["int" (append (list (Declaration (ty-Int) (Identifier dec-name))) (make-declarations r))]
       ["bool" (append (list (Declaration (ty-Bool) (Identifier dec-name))) (make-declarations r))]
       ["int_array" (append (list (Declaration (ty-Array) (Identifier dec-name))) (make-declarations r))]
       [else (append (list (Declaration (ty-Struct (Identifier (cast dec-type String)) '())
                                        (Identifier dec-name)))
                     (make-declarations r))])]))
;;-------------------------------XXX------------------------------------------------------





;;--------------------------------Make Functions AST-----------------------------------

;;Helper function to parse types
(define (parse-type [type : String]) : Type
  (match type
    ["int" (ty-Int)]
    ["bool" (ty-Bool)]
    ["void" (ty-Void)]
    ["int_array" (ty-Array)]
    [else (ty-Struct (Identifier type) '())]))

;;Function to setup function definitions in AST form
(define (make-functions [funcs : (Listof (HashTable Any Any))]) : Funcs-list
  (match funcs
    ['() '()]
    [(cons f r)
     (define func-name (cast (hash-ref f 'id) String))
     (define func-params (cast (hash-ref f 'parameters) (Listof (HashTable Any Any))))
     (define func-ret-type (cast (hash-ref f 'return_type) String))
     (define func-decls (cast (hash-ref f 'declarations) (Listof (HashTable Any Any))))
     ;;body is a list of statements
     (define func-body (cast (hash-ref f 'body) (Listof (HashTable Any Any))))
     (append (list (Func (Identifier func-name)
                         (make-declarations func-params)
                         (parse-type func-ret-type)
                         (make-declarations func-decls)
                         (parse-func-body func-body)))
             (make-functions r))]))

;;Function to parse the list of Statements(function body)
(define (parse-func-body [stmts : (Listof Any)]) : (Listof Statement)
  (match stmts
    ['() '()] 
    [(cons f r)
     (append (list (parse-stmt (cast f (HashTable Any Any)))) (parse-func-body r))]))

;;Helper function to parse target for assignment statment
(define (parse-lvalue [target : (HashTable Any Any)]) : Expression
  (match (hash-has-key? target 'left)
    [#t
     (match (hash-has-key? target 'index)
       [#t (let* ([left (cast (hash-ref target 'left) (HashTable Any Any))]
                  [ind (parse-expr (cast (hash-ref target 'index) (HashTable Any Any)))]
                  [left-expr (parse-lvalue left)])
             (IndexExpr left-expr ind))]
       [#f (let* ([left (cast (hash-ref target 'left) (HashTable Any Any))]
                  [id (Identifier (cast (hash-ref target 'id) String))]
                  [left-expr (parse-lvalue left)])
             (SelectorExpr left-expr id))])]
    [#f
     (Identifier (cast (hash-ref target 'id) String))])


  #;(match (hash-has-key? target 'left)
    [#t 
     (let* ([left (cast (hash-ref target 'left) (HashTable Any Any))]
            [id (Identifier (cast (hash-ref target 'id) String))]
            [left-expr (parse-lvalue left)])
       (SelectorExpr left-expr id))]
    [#f
     (Identifier (cast (hash-ref target 'id) String))])) 

;;Function to parse individual statements
(define (parse-stmt [stmt : (HashTable Any Any)]) : Statement
  (define stmt-type (hash-ref stmt 'stmt))
  (match stmt-type
    ["return"
     (cond
       [(hash-has-key? stmt 'exp)
        (Ret (parse-expr (cast (hash-ref stmt 'exp) (HashTable Any Any))))]
       [else (Ret (VoidLit))])
     ] 
    ["if"
     (Conditional (parse-expr (cast (hash-ref stmt 'guard) (HashTable Any Any)))
                  (cast (parse-stmt (cast (hash-ref stmt 'then) (HashTable Any Any))) Block)
                  (cond
                    [(hash-has-key? stmt 'else)
                     (cast (parse-stmt (cast (hash-ref stmt 'else) (HashTable Any Any))) Block)]
                    [else (Block '())])
                  #;(cast (parse-stmt (cast (hash-ref stmt 'then) (HashTable Any Any))) Block)
                  )]
    ["assign"
     (define source-val (parse-expr (cast (hash-ref stmt 'source) (HashTable Any Any))))
     (define target (cast (hash-ref stmt 'target) (HashTable Any Any)))
     (define target-val (parse-lvalue target))  ; parse-lvalue for nested target parsing
     (Assignment target-val source-val)
     ]
    ["print"
     (define endl (cast (hash-ref stmt 'endl) Boolean))
     (define exp (parse-expr (cast (hash-ref stmt 'exp) (HashTable Any Any))))
     (Print exp endl)]
    ["invocation"
     (define func-name (cast (hash-ref stmt 'id) String))
     (define args-raw (cast (hash-ref stmt 'args) (Listof (HashTable Any Any))))
     (define list-exprs (map parse-expr args-raw))
     (Invocation (Identifier func-name) list-exprs)]
    ["while"
     (define guard (parse-expr (cast (hash-ref stmt 'guard) (HashTable Any Any))))
     (define parsed-body (parse-stmt (cast (hash-ref stmt 'body) (HashTable Any Any))))
     (Loop guard (cast parsed-body Block))]
    ["delete"
     (define expr (parse-expr (cast (hash-ref stmt 'exp) (HashTable Any Any))))
     (Delete expr)]
    ["block"
     (Block (parse-func-body (cast (hash-ref stmt 'list) (Listof Any))))]))

;;Function to parse single expression
(define (parse-expr [expr : (HashTable Any Any)]) : Expression
  (define expr-type (cast (hash-ref expr 'exp) String))
  (match expr-type
    ["num" (NumLit (cast (string->number (cast (hash-ref expr 'value) String)) Integer))]
    ["dot" (SelectorExpr 
            (parse-expr (cast (hash-ref expr 'left) (HashTable Any Any)))
            (Identifier (cast (hash-ref expr 'id) String)))]
    ["id" (Identifier (cast (hash-ref expr 'id) String))]  
    ["binary"
     (match (cast (hash-ref expr 'operator) String)
       ["+" (SimpleExpr 
             (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
             "+"
             (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["-" (SimpleExpr 
             (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
             "-"
             (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["*" (TermExpr 
             (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
             "*"
             (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["/" (TermExpr 
             (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
             "/" 
             (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))] 
       ["<" (RelExpr 
             (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
             "<"
             (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       [">" (RelExpr 
             (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
             ">"
             (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))] 
       [">=" (RelExpr 
              (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
              ">="
              (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["<=" (RelExpr 
              (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any))) 
              "<="
              (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["==" (EqExpr 
              (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
              "=="
              (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["!=" (EqExpr 
              (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
              "!="
              (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["&&" (BoolExpr 
              (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
              "&&"
              (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))]
       ["||" (BoolExpr 
              (parse-expr (cast (hash-ref expr 'lft) (HashTable Any Any)))
              "||"
              (parse-expr (cast (hash-ref expr 'rht) (HashTable Any Any))))
             ])]
    
    ["unary" 
     (let ([operator (cast (hash-ref expr 'operator) String)]
           [operand (parse-expr (cast (hash-ref expr 'operand) (HashTable Any Any)))])
       (match operator
         ["!" (UnaryExpr "!" operand)]
         ["-" (UnaryExpr "-" operand)]
         [else (error 'parse-expr "Invalid unary operator ~e\n" operator)]))]
    ["invocation" (Invocation 
                   (Identifier (cast (hash-ref expr 'id) String)) 
                   (map parse-expr (cast (hash-ref expr 'args) (Listof (HashTable Any Any)))))]
    ["new"
     (define type (cast (hash-ref expr 'id) String))
     (if (string=? type "int_array")
       (NewArray (cast (string->number (cast (hash-ref expr 'size) String)) Natural))
       (NewExpr (Identifier (cast (hash-ref expr 'id) String))))]
    ["null" (NullLit)] 
    ["true" (BoolLit #t)]
    ["false" (BoolLit #f)]
    ["index" (IndexExpr
              (parse-expr (cast (hash-ref expr 'left) (HashTable Any Any)))
              (parse-expr (cast (hash-ref expr 'index) (HashTable Any Any))))]
    ["read" (Read)]
    [else (error 'parse-expr "Invalid expression type given ~e\n" expr-type)]))
;;-------------------------------------XXX-------------------------------------------------





;;-------------------------------------Build Complete AST from JSON------------------------
;; Function to read JSON from a file and return it as a Racket data structure
(define (read-json-from-file [path : String]) : Any
  (with-input-from-file path read-json))

(define (main [json-file-path : String])
  (define json-data (read-json-from-file json-file-path))
  ;; check if the read-json returned a hashtable
  (cond
    [(not (hash? json-data)) (error "Hash not read from json\n")])

  ;; fetch types, decls, and functions hash tables- these are all lists
  (define types (cast (hash-ref json-data 'types) (Listof (HashTable Any Any))))
  (define declarations (cast (cast (hash-ref json-data 'declarations) (Listof Any)) (Listof (HashTable Any Any))))
  (define functions (cast (hash-ref json-data 'functions) (Listof (HashTable Any Any))))

  ;;transform json ast to my own ast
  (define ast-types (make-types types))
  (define ast-decls (make-declarations declarations))
  (define ast-functions (make-functions functions))
  
  ;;complete ast
  (Program ast-types ast-decls ast-functions))
   
#;(define top-prog (main "/Users/shravansheth/Desktop/calpoly/431/mile1/given_parser/wasteOfCycles.json"))
(define top-prog (main (vector-ref (current-command-line-arguments) 0)))
;;top-prog
;;-------------------------------------XXX-------------------------------------------------





;;-------------------------------------Typecheck AST---------------------------------------
;; Global symbol table stack (initially contains the global scope)
(define global-scope (make-hash))
(define symbol-table-stack (list global-scope))

(define (enter-scope)
  (set! symbol-table-stack (cons (make-hash) symbol-table-stack)))

(define (exit-scope)
  (set! symbol-table-stack (cdr symbol-table-stack)))

(define (current-scope)
  (first symbol-table-stack))

;; Global types-hash
(define types-hash : (Mutable-HashTable (U Symbol String) (U Integer (Listof Any)))
  (make-hash '((ty-Int . 0)
               (ty-Bool . 0)
               ("int_array" . 0))))

;; add struct type to types hash
(define (add-struct-types [struct-list : Types-list]) : Void
  (match struct-list
    ['() (void)]
    [(cons f r)
     (define struct-id (Identifier-name (ty-Struct-id (cast f ty-Struct))))
     ;; check if struct name already used
     (cond
       [(hash-has-key? types-hash struct-id)
        (error 'Typechecker "Struct name already used ~e\n" struct-id)])
     (hash-set! types-hash (cast struct-id String) (ty-Struct-fields (cast f ty-Struct)))
     (add-struct-types r)]))  

;; Validate the fields of a struct
(define (validate-field-types [fields : (Listof NestedField)]) : Void
  (match fields
    ['() (void)]
    [(cons f r)
     (define field-type (NestedField-type f))
     (match field-type
       [(ty-Int) (validate-field-types r)]
       [(ty-Bool) (validate-field-types r)]
       [(ty-Struct type _)
        (define struct-type (Identifier-name type))
        (cond
          [(hash-has-key? types-hash struct-type)
           (validate-field-types r)]
          [else (error 'Typechecker "Invalid type of field ~e\n" field-type)])]
       [(ty-Void) (validate-field-types r)]
       [(ty-Array) (validate-field-types r)]
       [else (error 'Typechecker "Invalid type of field ~e\n" field-type)])]))

;;Function to validate a single type
(define (validate-type [ty : Type])
  (match ty
    [(ty-Int) #t]
    [(ty-Bool) #t]
    [(ty-Struct type _) 
     (define struct-type (Identifier-name type))
     (cond
       [(hash-has-key? types-hash struct-type) #t]
       [else (error 'Typechecker "Invalid type of field ~e\n" struct-type)])]
    [(ty-Void) #t]
    [(ty-Array) #t]
    [else (error 'Typechecker "Invalid type used ~e\n" ty)]))

;; build global stack symbol table
(define (build-scope [decl-list : Decls-list] [scope : (HashTable Any Any)]) : Void
  (match decl-list
    ['() (void)]
    [(cons f r)
     (define dec-type (Declaration-type f))
     (validate-type dec-type)
     (define dec-id (Declaration-id f))
     (add-identifier dec-id dec-type scope)
     (build-scope r scope)]))

;;add variable to current scope
(define (add-identifier [id : Identifier] [type : Type] [scope : (HashTable Any Any)])
  (hash-set! scope (Identifier-name id) type))


;;Function to process function parameters
;; make new scope for the function
(define (process-function-parameters [func-decl : Func])
  (enter-scope)
  (for-each (lambda (param)
              (let* ([param-id (Identifier-name (Declaration-id (cast param Declaration)))]
                     [param-type (Declaration-type (cast param Declaration))])
                (add-identifier (Declaration-id (cast param Declaration)) param-type (current-scope))))
            (Func-parameters func-decl)))

;; Function to process local function declarations
(define (process-local-declarations [decls : (Listof Declaration)])
  (for-each (lambda (decl)
              (let* ([decl-id (Identifier-name (Declaration-id (cast decl Declaration)))]
                     [decl-type (Declaration-type (cast decl Declaration))])
                (when (hash-has-key? (current-scope) decl-id)
                  (error 'Typechecker "Local variable redeclaration: ~a" decl-id))
                (add-identifier (Declaration-id (cast decl Declaration)) decl-type (current-scope))))
            decls))

#;(define (process-declarations decls)
  (for-each (lambda ([decl : Declaration])
              (let* ([decl-id (Identifier-name (Declaration-id decl))]
                     [decl-type (Declaration-type decl)])
                (when (hash-has-key? (current-scope) decl-id)
                  (error 'Typechecker "Local variable redeclaration: ~a" decl-id))
                (add-identifier (Declaration-id decl) decl-type (current-scope))))
            decls))


(define (is-valid-type? x) : Boolean
  (or (ty-Int? x)
      (ty-Bool? x)
      (ty-Struct? x)
      (ty-Array? x)
      (ty-Void? x)
      ;;special case for a function signature on stack
      (list? x)))

;;Function to lookup an identifier in the stack
(define (search-scopes [scopes : (Listof (HashTable Any Any))] [id : String]) : Any
  (cond
    [(null? scopes) (error 'Typechecker "Identifier not in scope ~a" id)]
    [else
     (let* ([current-scope (first scopes)]
            [identifier-type (hash-ref current-scope id #f)])
       (if identifier-type
           identifier-type
           (search-scopes (rest scopes) id)))]))

(define (lookup-identifier [id : String])
  (search-scopes symbol-table-stack id))


;; HashTable for all function signatures only
(define functions-hash (make-hash))


;;Function to add function name to stack
(define (all-functions [func-list : Funcs-list])
  (define (add-function-signature [func-decl : Func])
    (define func-name (Identifier-name (Func-id func-decl)))
    (define param-types (map Declaration-type (Func-parameters func-decl)))
    (define ret-type (Func-ret-type func-decl))
  
    ;; Check for redeclaration of function
    (when (hash-has-key? (current-scope) func-name)
      (error 'Typechecker "Function redeclaration: ~a" func-name))
  
    ;; Register function signature in the current scope
    (hash-set! (current-scope) func-name (list 'Function param-types ret-type))
    (hash-set! functions-hash func-name (list 'Function param-types ret-type)))
  (map add-function-signature func-list))


;;check arguments given with function parameters
(define (check-arg-types [arg-exprs : (Listof Expression)] [param-types : (Listof Type)]) : Void
  (unless (equal? (length arg-exprs) (length param-types))
    (error 'Typechecker "The number of arguments does not match the number of parameters"))
  (for-each (lambda (arg-expr [param-type : Type]) 
              (let ([arg-type (check-expr (cast arg-expr Expression))])
                (unless (type-compatible? arg-type param-type)
                  (error 'Typechecker "Argument type ~a does not match parameter type ~a" arg-type param-type))))
            arg-exprs
            param-types))

;; Checks if a type is a struct type
(define (struct-type? type)
  (match type
    [(ty-Struct _ _) #t]
    [else #f]))


;; Checks if two types are compatible
(define (type-compatible? [type1 : Type] [type2 : Type]) : Boolean
  (match* (type1 type2)
    ;; Two primitive types are compatible if they are the same
    [((ty-Int) (ty-Int)) #t]
    [((ty-Bool) (ty-Bool)) #t]
    [((ty-Void) (ty-Void)) #t]

    ;; Null (ty-Void) is compatible with any struct type
    [((ty-Void) (ty-Struct _ _)) #t]
    [((ty-Struct _ _) (ty-Void)) #t] 

    ;; Struct types are compatible if their identifiers match
    [((ty-Struct id1 _) (ty-Struct id2 _)) (equal? (Identifier-name id1) (Identifier-name id2))]

    ;; Array types are compatible
    [((ty-Array) (ty-Array)) #t]

    [(_ _) #f]))
 

 
(define (check-expr [expr : Expression]) : Type
  (match expr
    [(NumLit _) (ty-Int)]
    [(BoolLit _) (ty-Bool)]
    [(NullLit) (ty-Void)]
    [(VoidLit) (ty-Void)]
    [(Read) (ty-Int)]
    [(NewArray size) (if (natural? size)
                         (ty-Array)
                         (error "Array size must be a Natural\n"))]
    [(NewExpr type-id)
     ;;(displayln type-id)
     ;; Ensure the type-id refers to a defined struct type
     (if (hash-has-key? types-hash (Identifier-name type-id))
         (begin
           (let ([new-type (hash-ref types-hash (Identifier-name type-id))])
             (cond
               [(list? new-type)
                (if (NestedField? (first new-type))
                    (ty-Struct type-id '())
                    (error 'Typechecker "New can only be used for structs\n"))]
               [(string=? (Identifier-name type-id) "int_array") (ty-Array)]
               [else (error 'Typechecker "You can only call new with structs or arrays\n")]))
           #;(let ([struct-type (hash-ref types-hash (Identifier-name type-id))])
               (if (list? struct-type)
                   (if (NestedField? (first struct-type))
                       (ty-Struct type-id '())
                       (error 'Typechecker "New can only be used for structs\n"))
                   (error 'Typechecker "New can only be used for structs\n"))))
         (error 'Typechecker "~e is an invalid type\n" (Identifier-name type-id)))]
    [(BoolExpr left op right)
     (let ([left-type (check-expr left)]
           [right-type (check-expr right)])
       (unless (member op '("&&" "||"))
         (error 'Typechecker "Invalid equality operator: ~a" op))
       (unless (and (ty-Bool? left-type) (ty-Bool? right-type))
         (error 'Typechecker "Boolean expressions require boolean operands"))
       (ty-Bool))]
    [(EqExpr left operator right)
     (let ([left-type (check-expr left)]
           [right-type (check-expr right)])
       ;; Check if the operator is one of the equality operators
       (unless (member operator '("==" "!="))
         (error 'Typechecker "Invalid equality operator: ~a" operator))
       ;; Ensure operands are of integer or structure type
       (unless (or (and (ty-Int? left-type) (ty-Int? right-type))
                   (and (ty-Struct? left-type)
                        (or (ty-Struct? right-type) (ty-Void? right-type))
                        (type-compatible? left-type right-type)))
         (error 'Typechecker "Equality operands must be both integers or both structures of the same type"))
       (ty-Bool))]
    [(RelExpr left operator right)
     (let ([left-type (check-expr left)]
           [right-type (check-expr right)])
       ;; Check if the operator is one of the relational operators
       (unless (member operator '("<" ">" "<=" ">=" "==" "&&" "||"))
         (error 'Typechecker "Invalid relational operator: ~a" operator))
       ;; Ensure both operands are integers
       (unless (and (ty-Int? left-type) (ty-Int? right-type))
         (error 'Typechecker "Relational expressions require integer operands")) 
       (ty-Bool))]
    [(SimpleExpr left operator right) 
     (let ([left-type (check-expr left)]
           [right-type (check-expr right)])
       ;; Check if the operator is + or -
       (unless (member operator '("+" "-"))
         (error 'Typechecker "SimpleExpr requires operator to be + or -, found: ~a" operator))
       ;; Ensure both operands are integers
       (unless (and (ty-Int? left-type) (ty-Int? right-type))
         (error 'Typechecker "Simple expressions (+, -) require integer operands"))
       (ty-Int))]
    [(TermExpr left operator right)
     (let ([left-type (check-expr left)]
           [right-type (check-expr right)]) 
       ;; Check if the operator is * or /
       (unless (member operator '("*" "/"))
         (error 'Typechecker "TermExpr requires operator to be * or /, found: ~a" operator))
       ;; Ensure both operands are integers
       (unless (and (ty-Int? left-type) (ty-Int? right-type))
         (error 'Typechecker "Term expressions (*, /) require integer operands"))
       (ty-Int))]
    [(UnaryExpr operator expr)
     (let ([expr-type (check-expr expr)])
       (match operator
         ["!" (unless (ty-Bool? expr-type)
                (error 'Typechecker "Operator ! requires a boolean operand")) (ty-Bool)]
         ["-" (unless (ty-Int? expr-type) (error "Unary operator - requires an integer operand")) (ty-Int)]
         [else (error 'Typechecker "Unsupported unary operator: ~a" operator)]))]
    [(SelectorExpr expr selector)
     (let* ([expr-type (check-expr expr)]
            [selector-name (Identifier-name selector)])
       (match expr-type
         [(ty-Struct struct-id _)
          (let* ([struct-fields (cast (hash-ref types-hash (Identifier-name struct-id)) (Listof NestedField))]
                 [field-type (lookup-field-type struct-fields selector-name)]
                 )
            (unless field-type
              ;;(displayln struct-fields) 
              (error 'Typechecker "Field ~a does not exist in structure ~a" selector-name (Identifier-name struct-id)))
            (cast field-type Type))]
         [else (error 'Typechecker "Selector expressions require a structure type on the left, got ~a" expr-type)]) )]
    [(Identifier id)
     (let ([id-type (lookup-identifier id)])
       (unless id-type
         (error 'Typechecker "Undefined identifier: ~a" id))
       (cast id-type Type))] 
    [(Invocation id args)
     ;;(displayln (lookup-identifier (Identifier-name id)))
     (let* ([func-signature (cast (search-scopes (list global-scope) (Identifier-name id)) (Listof Any))]
            [param-types (cast (if (and func-signature (eq? (first func-signature) 'Function))
                             (second func-signature)  ; param-types list
                             (error 'Typechecker "Function not defined: ~a" (Identifier-name id))) (Listof Type))]
            [return-type (if (and func-signature (eq? (first func-signature) 'Function))
                             (third func-signature)  ; return-type
                             (error  'Typechecker "Function not defined: ~a" (Identifier-name id)))]) 
       (unless (check-arg-types args param-types)
         (error 'Typechecker "Argument types do not match the function signature for: ~a" (Identifier-name id)))
       (if (is-valid-type? return-type)
           (cast return-type Type)
           (error 'Typechecker "Not a valid type returned\n")))]
    [(IndexExpr left index)
     (define left-type (check-expr left))
     (define index-type (check-expr index))
     (unless (and (ty-Int? index-type) (ty-Array? left-type))
       (error 'Typechecker "Left needs to be an array and the index has to be an integer\n"))
     (ty-Int)]
    [else
     (displayln expr)
     (error 'Typechecker "Unsupported expression type")]))  
 
 
;;Function to lookup if a field exists in a list of fields
(define (lookup-field-type [fields-list : (Listof NestedField)] [id : String]) : (U Type Boolean)
  (match fields-list
    ['() #f]
    [(cons f r)
     (define field-name (Identifier-name (NestedField-id f))) 
     (cond
       [(string=? field-name id)
        (NestedField-type f)]
       [else (lookup-field-type r id)])]))  


;;Check lvalue for assignment statement
(define (check-lvalue lvalue) : Type
  (match lvalue
    [(Identifier id)
     (define ty (lookup-identifier id))
     (if (is-valid-type? ty)
         (cast ty Type)
         (error 'Typechecker "Not a valid type returned\n"))] 
    [(SelectorExpr _ _)
     (check-expr lvalue)]
    [(IndexExpr left index)
     (check-expr lvalue)]
    [else (error 'Typechecker "Invalid lvalue: ~a" lvalue)])) 

;;Process list of statements
(define (process-statements-list [stmts : (Listof Statement)] [ret-type : Type])
  (for-each (λ ([stmt : Statement]) (process-statement stmt ret-type)) stmts))


 
;;Function to process individual statements
(define (process-statement [stmt : Statement] [ret-type : Type]) : Void
  (match stmt
    [(Assignment target source)
     (let ([target-type (check-lvalue target)]
           [source-type (check-expr source)])
       (unless (type-compatible? target-type source-type)
         (error 'Typechecker "Type mismatch in assignment- ~a - ~a" target-type source-type)))]
    [(Print expr endl)
     (let ([expr-type (check-expr expr)])
       (unless (ty-Int? expr-type)
         (error 'Typechecker "Print statement requires an integer expression")))]
    [(Conditional guard then-block else-block)
     (let ([guard-type (check-expr guard)])
       (unless (ty-Bool? guard-type)
         (error 'Typechecker "Conditional guard expression must be boolean"))
       (process-statements-list (Block-stmts then-block) ret-type)
       (process-statements-list (Block-stmts else-block) ret-type))]
    [(Loop guard body)
     (let ([guard-type (check-expr guard)])
       (unless (ty-Bool? guard-type)
         (error 'Typechecker "Loop guard expression must be boolean"))
       (process-statements-list (Block-stmts body) ret-type))]
    [(Ret expr)
     (let ([expr-type (check-expr expr)])
       (unless (type-compatible? expr-type ret-type)
         (error 'Typechecker "Return expression type does not match function return type")))]
    [(Block stmts)
     (process-statements-list stmts ret-type)]
    [(Delete expr)
     (let ([expr-type (check-expr expr)])
       (unless (ty-Struct? expr-type)
         (error 'Typechecker "Delete statement requires a structure type expression")))]
    [(Invocation id args) 
     (let* ([func-signature (cast (search-scopes (list global-scope) (Identifier-name id)) (Listof Any))])
       (unless (and func-signature (eq? (first (cast func-signature (Listof Any))) 'Function))
         (error 'Typechecker "Function not defined: ~a" (Identifier-name id)))
       (let ([param-types (cast (second func-signature) (Listof Type))]
             [return-type (third func-signature)])
         (unless (check-arg-types args param-types)
           (error 'Typechecker "Argument types do not match the function signature for: ~a" (Identifier-name id))
           return-type)))]
    [else (error 'Typechecker "Invalid type of statement\n")])) 


(define (analyze-function-returns [func-decl : Func])
  (define ret-type (Func-ret-type func-decl))
  (define statements (Func-statements func-decl))

  (define (analyze-statements [stmts : (Listof Statement)]) : Symbol 
    (match stmts
      ['() 'incomplete]  ; End of statements without a return
      [(cons (Ret ret-expr) rest)
       (let ([expr-type (check-expr ret-expr)])
         ;; Ensure return type matches expected.
         (unless (type-compatible? expr-type ret-type)
           (error 'Typechecker "Return type mismatch: expected ~a, got ~a" ret-type expr-type))
         'complete)]
      [(cons (Conditional test then-branch else-branch) rest)
       (let ([then-result (analyze-statements (Block-stmts then-branch))]
             [else-result (analyze-statements (Block-stmts else-branch))])
         (match (list then-result else-result)
           ['(complete complete) 'complete]  
           [_ (analyze-statements rest)]))]
      [(cons _ rest) (analyze-statements rest)]))

  (let ([result (analyze-statements statements)])
    (unless (or (eq? result 'complete) (ty-Void? ret-type))
      (error 'Typechecker "Function declared with non-void return type but doesn't return on all paths."))))

 

(define (process-complete-function [fun : Func])
  (when (string=? "main" (Identifier-name (Func-id fun)))
    (when (not (ty-Int? (Func-ret-type fun)))
      (error 'Typechecker "Main must return an integer.\n")))
  (process-function-parameters fun)
  (process-local-declarations (Func-declarations fun))

  ;;process body of function
  (process-statements-list (Func-statements fun) (Func-ret-type fun)) 

  ;;Check return paths
  (analyze-function-returns fun))

(define (typecheck [prog : Program])
  ;;process all the struct definitions(types)
  (add-struct-types (Program-types prog))
  ;;process all global variables 
  (build-scope (Program-declarations prog) global-scope)
  ;;process functions
  ;; steps- enter scope- , process local declarations of,
  ;; process body, check return paths, exit scope
  ;;
  ;;check if main exists
  (all-functions [Program-functions prog])
  (when (not (hash-has-key? global-scope "main"))
    (error 'Typechecker "No main function found.\n"))
  ;;call the function to process functions
  (for-each process-complete-function (Program-functions prog))
  (printf "Typecheck successfully completed.\n"))

(typecheck top-prog)

;;(hash-ref types-hash "B")
;;(lookup-identifier "bbb")
;;-------------------------------------XXX-------------------------------------------------
(provide (all-defined-out))