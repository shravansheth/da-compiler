#lang typed/racket
(require "typecheck.rkt")
(require racket/format)

;;------------------------------------------------------CFG-structs-----------------------------------------------------
(define current-label-counter 0)

(struct BasicBlock
  ([label : String] ; A unique identifier for the block, used for branching and labeling.
   [instructions : (Listof LLVM-Instruction)]
   [next/then : (U BasicBlock #f)]
   [else : (U BasicBlock #f)]
   [preds : (Listof BasicBlock)]
   [block-env : BlockEnv]
   [sealed : Boolean]
   [incomplete-phis : (HashTable String Phi)]) ; A list of strings representing LLVM instructions.
  #:transparent
  #:mutable)

(struct FuncBlock
  ([label : String]
   [ret-type : Type]
   [parameters : (Listof Declaration)]
   [local : (Listof Declaration)]
   [header : String]
   [body : BasicBlock]
   [blocks : (Listof BasicBlock)])
  #:transparent
  #:mutable)

;;------------------------------------------------------XXX-------------------------------------------------------------

;; Function to increment the counter and return the new value as a string
(define (increment-counter)
  (set! current-label-counter (+ current-label-counter 1))
  (number->string current-label-counter))


;; Function to create a new BasicBlock with a unique label
(define (create-new-bb) : BasicBlock
  (let ([new-label (increment-counter)])  ; Increment counter and convert to string for label
    (BasicBlock new-label '() #f #f '() (make-hash) #f (make-hash))))  ; Empty instructions list, no next/then or else block yet


;; Returns the last block in the sequence for chaining purposes.
(define (last-block [blk : BasicBlock]) : BasicBlock
  (let ([next-thn (BasicBlock-next/then blk)])
    (if (not (BasicBlock-else blk))  ; Explicitly check if next/then is not false
        blk
        (if (BasicBlock? next-thn)
            (last-block next-thn)
            (error "Not a BasicBlock\n")))))

;;------------------------------------------------------LLVM-structs----------------------------------------------------
(struct Register ([reg : Symbol]
                  [type : String]
                  #;[version : Natural])
  #:transparent)

(define-type LLVM-Instruction (U Alloca
                                 Bitcast
                                 Store
                                 Load
                                 BinaryOp
                                 RetInstr
                                 CondBr
                                 Br
                                 GetElementPtrInstr
                                 CallInstr
                                 PrintInstr
                                 ScanfInstr
                                 MallocInstr
                                 TypeInstr
                                 GlobalInstr
                                 FreeInstr
                                 Phi
                                 ))

;; Alloca instruction for memory allocation on the stack
(struct Alloca ([result : Register]
                [type : String])
  #:transparent)

(struct Bitcast ([result-reg : Register]
                 [source-reg : Register]
                 [cast-to : String])
  #:transparent)

;; Store instruction for writing a value to a memory address
(struct Store ([value : (U Register String)]
               [where : Register])
  #:transparent
  #:mutable)

;; Load instruction for reading from a memory address
(struct Load ([result : Register]
              [pointer : Register])
  #:transparent
  #:mutable)

;; Binary operation instructions (Add, Sub, Mul, Div)
(struct BinaryOp ([op : Symbol]
                  [result : Register]
                  [operand1 : (U Register String )]
                  [operand2 : (U Register String )])
  #:transparent
  #:mutable)

;; Return instruction
(struct RetInstr ([value : (U String Register)])
  #:transparent
  #:mutable)

;; Conditional branch instruction
(struct CondBr ([condition : (U Register String)]
                [true-label : String]
                [false-label : String])
  #:transparent
  #:mutable)

;; Unconditional branch instruction
(struct Br ([label : String])
  #:transparent)

(struct GetElementPtrInstr ([result : Register]
                            [ptr : Register]
                            [index : (U Register String)])
  #:transparent
  #:mutable)

(struct CallInstr ([result : (U Register String)]
                   [func : String]
                   [args : (Listof (U Register String))])
  #:transparent
  #:mutable)

(struct PrintInstr
  ([value : (U Register String)]
   [endl : Boolean])
  #:transparent
  #:mutable)

(struct ScanfInstr
  ([result-reg : Register])
  #:transparent)

(struct MallocInstr
  ([result-reg : Register]
   [size : Natural])
  #:transparent)

(struct TypeInstr
  ([result-reg : Register]
   [sizes : (Listof String)])
  #:transparent
  #:mutable)

(struct GlobalInstr
  ([result-reg : Register])
  #:transparent
  #:mutable)

(struct FreeInstr
  ([result-reg : Register])
  #:transparent)

(struct Binding
  ([id : String]
   [reg : Register])
  #:transparent)

(struct BlockBind
  ([sym : String]
   [val : (U Register String)])
  #:transparent)

(define-type BlockEnv (HashTable Any Any))

(define-type Scope (Listof Binding))

#;(define-type BlockList (HashTable Any Any))

(struct Phi
  ([result-reg : Register]
   [values : (Listof (U Register String))]
   [blocks : (Listof String)])
  #:transparent
  #:mutable)

(define all-blocks : (HashTable String BasicBlock)
  (make-hash))

;;------------------------------------------------------XXX-------------------------------------------------------------




;;------------------------------------------------------LLVM-processing-------------------------------------------------
;; helper function to generate LLVM type
(define (llvm-type ty) : String
  (match ty
    [(ty-Int) "i64"]
    [(ty-Bool) "i1"]
    [(ty-Array) "i64*"]
    [(ty-Void) "void"]
    [(ty-Struct struct-id _)
     (format "%struct.~a*" (Identifier-name struct-id))]
    [else (error "Invalid type ~a \n" ty)]))

;; Add * to a register name
(define (make-ptr [ty : String]) : String
  (format "~a*" ty))

;; Function to remove * from a string
(define (remove-ptr [s : String]) : String
  (if (string=? "*" (substring s (sub1 (string-length s))))
      (substring s 0 (sub1 (string-length s)))
      s
      #;(error "Not a pointer\n")))

;; Helper function to generate unique symbol names for LLVM registers
(define register-count 0)
(define (make-register [ty : String])
  (define reg (Register (string->symbol (format "%r~a" register-count)) ty))
  (set! register-count (+ register-count 1))
  reg)

;; function to append an instruction to a basic block
(define (append-instruction! [block : BasicBlock] [instr : LLVM-Instruction])
  (set-BasicBlock-instructions! block (append (BasicBlock-instructions block) (list instr))))

;; Build local scope 
(define (build-func-scope [declarations : (Listof Declaration)] [global : Boolean]) : Scope
  (map (lambda ([decl : Declaration])
         (Binding
          (Identifier-name (Declaration-id decl))
          (if global
              (Register (string->symbol (format "@~a" (Identifier-name (Declaration-id decl))))
                    (make-ptr (llvm-type (Declaration-type decl))))
              (Register (string->symbol (format "%~a" (Identifier-name (Declaration-id decl))))
                    (make-ptr (llvm-type (Declaration-type decl))))))) 
       declarations))

;; Lookup an id in the function scope
(define (lookup-scope [id : String] [scope : Scope]) : Register
  (match scope
    ['() (error (format "Variable not in scope ~a\n" id))]
    [(cons f r)
     (if (string=? id (Binding-id (cast f Binding)))
         (Binding-reg (cast f Binding))
         (lookup-scope id r))]))

;; Write the alloca instruction
(define (translate-allocas [decls : Scope] [block : BasicBlock]) : Void
  (for-each (lambda ([reg : Binding])
              (when (not (string=? (remove-ptr (Register-type (Binding-reg reg))) "void"))
                (define alloca-inst (Alloca (Binding-reg reg) (remove-ptr (Register-type (Binding-reg reg)))))
                (append-instruction! block alloca-inst)))
            decls))

;; Function to check if returned value is a register
(define (register? x)
  (Register? x))

;; Function to check a function's return type (incase info not avaiable)
(define (fun-ret-type [id : String] [funcs : Funcs-list]) : Type
  (match funcs
    ['() (error "Function not found")]
    [(cons f r)
     (if (string=? id (Identifier-name (Func-id f)))
         (Func-ret-type f)
         (fun-ret-type id r))]))

;; Function to get the index of a field in a struct
(define (field-index [fields-list : (Listof NestedField)] [field : String] [counter : Natural]) : Natural
  (match fields-list
    ['() (error "Field not found ~a\n" field)]
    [(cons f r)
     
     (if (string=? (Identifier-name (NestedField-id f)) field)
         counter
         (field-index r field (+ counter 1)))]))

;; Function to get the type of a field in a struct
(define (field-ty [fields-list : (Listof NestedField)] [field : String]) : Type
  (match fields-list
    ['() (error "Field not found\n")]
    [(cons f r)
     (if (string=? (Identifier-name (NestedField-id f)) field)
         (NestedField-type f)
         (field-ty r field))])) 

;; Function to get the name of the struct from register name (don't know how useful)
(define (extract-name [struct-str : String]) : String
  ;; Split the string by '.'
  (define parts (string-split struct-str "."))
  ;; Check if parts exceeded
  (if (< 1 (length parts))
      ;; Further process the second part to remove '*' and return it
      (string-trim (second parts) "*")
      ;; Return an empty string if the format is not as expected
      (error "Couldn't extract struct name\n")))

(define (translate-f-args [args : (Listof Expression)]
                          [fname : Identifier]
                          [block : BasicBlock]
                          [f-scope : Scope]) : (Listof (U Register String))
  (define counter -1)
  (define params
    (cast (second (cast #;(lookup-identifier (Identifier-name fname))
                        (hash-ref functions-hash (Identifier-name fname))
                        (Listof Any))) (Listof Any)))
  (map (lambda ([arg : Expression])
         (set! counter (+ counter 1))
         (let* ([trans-arg (translate-expr arg block f-scope #f)])
           (if (and (string? trans-arg)
                    (string=? trans-arg "null"))
               (format "~a null" (llvm-type (list-ref params counter)))
               trans-arg))
         ) 
       args)) 

;; Function to add phi operands to the phi instruction
(define (add-phi-ops [var : String] [phi : Phi] [preds : (Listof BasicBlock)] [ty : String]) : Void
  (for-each (lambda ([block : BasicBlock])
              (let* ([pred-val (find-id var block ty)]) 
                (unless #;(member pred-val (Phi-values phi)) #f
                  (when (not (and (string? pred-val) (string=? pred-val "undefined")))
                    (begin
                      (set-Phi-values! phi (append (Phi-values phi) (list pred-val)))
                      (set-Phi-blocks! phi (append (Phi-blocks phi) (list (BasicBlock-label block)))))))))
            preds))


;; Function to seal a block and incomplete phis
(define (seal-block [block : BasicBlock]) : Void 
  (hash-for-each (BasicBlock-incomplete-phis block) 
                 #;(lambda (key val)
                   (add-phi-ops (cast key String)
                                (cast val Phi)
                                (BasicBlock-preds block)
                                (Register-type (Phi-result-reg (cast val Phi)))))
                 (lambda ([key : String] [val : Phi])
                   (add-phi-ops key
                                val
                                (BasicBlock-preds block)
                                (Register-type (Phi-result-reg (cast val Phi))))))
  (set-BasicBlock-sealed! block #t)
  (void))


;; Function to find a variable in a block env
(define (find-id [var : String]
                 [block : BasicBlock]
                 [ty : String]
                 )
  : (U Register String)
  (if (hash-has-key? (BasicBlock-block-env block) var)
      (begin
        (let* ([res (hash-ref (BasicBlock-block-env block) var)])
          (if (string? res)
              (if (string-contains? res "null")
                  "null"
                  res)
              (cast res Register))))
      ;;else call function to check preds blocks
      (begin
        (let* ([val (check-preds var block ty)])
          (if (and (string? val) (string-contains? val "null"))
          "null"
          val))
        )))

;; Function to find a variable in predecessors of a block
(define (check-preds [var : String] [block : BasicBlock] [ty : String]) : (U Register String)
  (if (not (BasicBlock-sealed block))
      (let* ([val (Phi (make-register ty) '() '())])
        #;(set-BasicBlock-incomplete-phis! block
                                         (hash-set! (BasicBlock-incomplete-phis block) var val))
        (hash-set! (BasicBlock-incomplete-phis block) var val)
        (hash-set! (BasicBlock-block-env block) var (Phi-result-reg val))

        (set-BasicBlock-instructions! block (cons val (BasicBlock-instructions block)))
        (Phi-result-reg val))
      ;; else if- no preds
      (if (eq? (length (BasicBlock-preds block)) 0)
          (let* ([val "undefined"])
            (hash-set! (BasicBlock-block-env block) var val)
            ;; SHOULD IT ERROR HERE????????
            val
            )
          ;; else if- 1 pred 
          (if (eq? (length (BasicBlock-preds block)) 1)
              (let* ([val (find-id var
                                    (hash-ref all-blocks
                                              (BasicBlock-label (first (BasicBlock-preds block))))
                                   ty)]) 
                (hash-set! (BasicBlock-block-env block) var val)
                val)
              ;; else- multiple preds
              (let* ([val (Phi (make-register ty) '() '())])
                (hash-set! (BasicBlock-block-env block) var (Phi-result-reg val))
                
                (set-BasicBlock-instructions! block (cons val (BasicBlock-instructions block)))
                (add-phi-ops var val (BasicBlock-preds block) ty) 
                (Phi-result-reg val))
              )
          ) 
      ))

;; Function to add function params to block env
(define (add-func-params [block : BasicBlock] [params : (Listof Declaration)]) : Void
  (define counter 0)
  (for-each (lambda ([decl : Declaration]) 
              (append-bind! block (Identifier-name (Declaration-id decl)) (Register
                                                                           (string->symbol (format "%~a" counter))
                                                                           (llvm-type (Declaration-type decl))))
              (set! counter (+ counter 1)))
            params))



;; Translate an expression into LLVM and add the resulting instruction to the given block
(define (translate-expr
         [expr : Expression]
         [block : BasicBlock]
         [f-scope : Scope]
         [l-value : Boolean])
  : (U Register String)
  (match expr
    [(NumLit value)
     ;; Immediate numbers are returned as strings
     (format "~a" value)]
    [(BoolLit #t)
     (let* ([result-reg (make-register "i1")]
            [icmp-instr (BinaryOp 'eq-icmp  result-reg "true" "true")])
       (append-instruction! block icmp-instr)
       result-reg)]
    [(BoolLit #f)
     (let* ([result-reg (make-register "i1")]
            [icmp-instr (BinaryOp 'eq-icmp  result-reg "true" "false")])
       (append-instruction! block icmp-instr)
       result-reg)]
    [(NullLit)
     "null"]
    [(VoidLit) "void"] 
    #;[(Identifier id)
     (if l-value
         (begin
           (let ([ptr-reg (lookup-scope id f-scope)])
             ptr-reg))
         (begin
           (let* ([ty (remove-ptr(Register-type (lookup-scope id f-scope)))]
                  [result-reg (make-register ty)]
                  [source-reg (lookup-scope id f-scope)]
                  [load-instr (Load result-reg source-reg)])
             (append-instruction! block load-instr)
             result-reg)))]
    [(Identifier id) 
     (if l-value
         (if (string-contains? (symbol->string (Register-reg (lookup-scope id f-scope))) "@")
             (let ([ptr-reg (lookup-scope id f-scope)])
               ptr-reg)
             id)
         #;(begin
             (let ([ptr-reg (lookup-scope id f-scope)])
               ptr-reg))
         #;(let* ([ty (remove-ptr(Register-type (lookup-scope id f-scope)))]
                  [result-reg (make-register ty)]
                  [source-reg (lookup-scope id f-scope)]
                  [load-instr (Load result-reg source-reg)])
             (append-instruction! block load-instr)
             result-reg)
         (if (string-contains? (symbol->string (Register-reg (lookup-scope id f-scope))) "@")
             (let* ([ty (remove-ptr(Register-type (lookup-scope id f-scope)))]
                    [result-reg (make-register ty)]
                    [source-reg (lookup-scope id f-scope)]
                    [load-instr (Load result-reg source-reg)])
               (append-instruction! block load-instr)
               result-reg)
             (let* ([ty (remove-ptr(Register-type (lookup-scope id f-scope)))]
                    [val (find-id id block ty)])
               val)))]
    [(IndexExpr left index)
     ;; For array indexing: arr[index]
     (let* ([array-reg (cast (translate-expr left block f-scope #f) Register)] 
            [index (translate-expr index block f-scope #f)]
            [result-reg (make-register "i64*")])
       (define gep-instr (GetElementPtrInstr result-reg array-reg index))
       (append-instruction! block gep-instr)
       (if l-value
           result-reg
           (begin
             (let* ([value-reg (make-register "i64")]
                    [load-instr (Load value-reg result-reg)])
               (append-instruction! block load-instr)
               value-reg))))]
    [(BoolExpr left op right)
     (let* ([l (translate-expr left block f-scope #f)]
            [r (translate-expr right block f-scope #f)] 
            [result-reg (make-register "i1")]
            [op-symbol (if (equal? op "&&") 'and 'or)])
       (define instr (BinaryOp op-symbol result-reg l r))
       (append-instruction! block instr)
       result-reg)]
    [(EqExpr left op right) 
     ;; Equality and inequality comparison
     (let* ([l (translate-expr left block f-scope #f)]
            [r (translate-expr right block f-scope #f)]
            [result-reg (make-register "i1")]
            [op-symbol (if (equal? op "==") 'eq-icmp 'ne-icmp)])
       
       (define cmp-instr (BinaryOp op-symbol result-reg l r))
       (append-instruction! block cmp-instr)
       result-reg)]
    [(RelExpr left op right)
     ;; Relational expressions
     (let* ([l (translate-expr left block f-scope #f)]
            [r (translate-expr right block f-scope #f)]
            [result-reg (make-register "i1")]
            [op-symbol (case op
                         [("<") 'slt-icmp]
                         [(">") 'sgt-icmp]
                         [("<=") 'sle-icmp]
                         [(">=") 'sge-icmp]
                         [else (error "Invalid binary operation\n")])])
       (define cmp-instr (BinaryOp op-symbol result-reg l r))
       (append-instruction! block cmp-instr)
       result-reg)]
    [(SimpleExpr left op right)
     ;; Simple arithmetic expressions
     (let* ([l (translate-expr left block f-scope #f)]
            [r (translate-expr right block f-scope #f)]
            [result-reg (make-register "i64")]
            [op-symbol (case op
                         [("+") 'add]
                         [("-") 'sub]
                         [else (error "Invalid binary operation\n")])])
       (define arith-instr (BinaryOp op-symbol result-reg l r))
       (append-instruction! block arith-instr)
       result-reg)]
    [(TermExpr left op right)
     ;; Term expressions for multiplication and division
     (let* ([l (translate-expr left block f-scope #f)]
            [r (translate-expr right block f-scope #f)]
            [result-reg (make-register "i64")]
            [op-symbol (case op
                         [("*") 'mul]
                         [("/") 'sdiv])])
       (define term-instr (BinaryOp (cast op-symbol Symbol) result-reg l r))
       (append-instruction! block term-instr)
       result-reg)]
    [(UnaryExpr op expr)
     ;; Unary expressions for logical not and negation
     (let* ([exp (translate-expr expr block f-scope #f)]
            [result-reg
             (case op
               [("!") (make-register "i1")]
               [("-") (make-register "i64")]
               [else (error "Invalid unary operation\n")])]
            [instr
             (case op
               [("!") (BinaryOp 'xor result-reg exp "true")]
               [("-") (BinaryOp 'mul result-reg exp "-1")]
               [else (error "Invalid binary operation\n")])])
       (append-instruction! block instr)
       result-reg)]
    [(SelectorExpr expr selector)
     (let* (;; has to be a register, can't be a string
            [struct-reg (cast (translate-expr expr block f-scope #f) Register)] 
            [field-name (Identifier-name selector)]
            [struct-name (extract-name (Register-type struct-reg))]
            )

       ;; Struct reg- reg- '%struct.foo*  
       (define struct-fields (hash-ref types-hash struct-name)) ;; result- Listof NestedField

       (define field-ind (field-index (cast struct-fields (Listof NestedField)) field-name 0)) ;;result- Natural

        
       (define field-type (field-ty (cast struct-fields (Listof NestedField)) field-name)) ;;result - Type

       
       
       ;; make result-pointer register
       (define field-ptr (make-register (make-ptr (llvm-type field-type))))

       ;;(printf (format "~a\n" field-ptr))

         
         
       ;; Then, create a getelementptr instruction to get the address of the field
       ;;remove ptr of result-ptr while translating
       (define gep-instr (GetElementPtrInstr field-ptr struct-reg (number->string field-ind))) 

       ;; append instruction to the block
       (append-instruction! block gep-instr)

       (if l-value
           (begin
             field-ptr)
           (begin
             (let* ([field-val (make-register (llvm-type field-type))]
                    [load-instr (Load field-val field-ptr)])
               (append-instruction! block load-instr)
               field-val))))] 
    [(Invocation func-id args)
     ;; Process a function invocation
     (let* (
            [func-name (format "@~a" (Identifier-name func-id))]  ; Convert to LLVM function name
            [arg-exprs args]  ; Get the list of argument expressions
            #;[arg-results
             (map (lambda ([arg : Expression]) (translate-expr arg block f-scope #f)) arg-exprs)]
            [arg-results (translate-f-args arg-exprs func-id block f-scope)]
            [return-type (llvm-type (fun-ret-type (Identifier-name func-id) (Program-functions top-prog)))]
            ; Determine if a result register is needed
            [result (if (string=? return-type "void") "void" (make-register return-type))])
       (define call-instr (CallInstr result func-name arg-results))
       ;;(printf "this ran\n")
       (append-instruction! block call-instr)
       (if (register? result)
           result
           "void"))]
    [(Read)
     (set! register-count (+ register-count 1))
     (define result (make-register "i64"))
     (define read-instr (ScanfInstr result)) 
     (define load-instr (Load result (Register (string->symbol "@.read_scratch") "i64*")))
     (append-instruction! block read-instr)
     (append-instruction! block load-instr)
     result]
    [(NewArray size)
     (let* ([arr-ptr (make-register (format "[~a x i64]" size))]
            [alloca-instr (Alloca arr-ptr (Register-type arr-ptr))]
            [arr-ptr-ptr (make-register "i64*")]
            [bitcast-instr (Bitcast arr-ptr-ptr arr-ptr "i64*")])
       (append-instruction! block alloca-instr)
       (append-instruction! block bitcast-instr)
       arr-ptr-ptr)]
    [(NewExpr type)
     (let* ([size (cast (hash-ref struct-sizes (Identifier-name type)) Natural)]
            [result-malloc (make-register "i8")]
            [malloc-instr (MallocInstr result-malloc size)]
            [result-ptr (make-register (format "%struct.~a*" (Identifier-name type)))]
            [bitcast-instr (Bitcast result-ptr result-malloc (Register-type result-ptr))])
       (append-instruction! block malloc-instr)
       (append-instruction! block bitcast-instr)
       result-ptr)]
    ))


;; Translate a statement into LLVM
(define (translate-statements [stmts : (Listof Statement)]
                              [block : BasicBlock]
                              [func : Func]
                              [f-scope : Scope]
                              [ret-block : BasicBlock])
  : BasicBlock

  (match stmts
    ['() block]
    [(cons f r)
     (match f
       [(Block block-stmts)
        (define lst (translate-statements block-stmts block func f-scope ret-block))
        (translate-statements r lst func f-scope ret-block)]
       
       #;[(Assignment target source)
        ;; Handle assignment statement
        (let* ([target-expr (translate-expr target block f-scope #t)]
               [source-expr (translate-expr source block f-scope #f)] 
               [instr (Store source-expr (cast target-expr Register))])
          (append-instruction! block instr))
        (translate-statements r block func f-scope ret-block)]

       [(Assignment target source)
        ;; Handle assignment statement
        (define source-expr (translate-expr source block f-scope #f))
        
        (if (Identifier? target)
            (if (string-contains?
                 (symbol->string (Register-reg (lookup-scope (Identifier-name target) f-scope))) "@")
                (let* ([target-expr (translate-expr target block f-scope #t)]
                       #;[source-expr (translate-expr source block f-scope #f)] 
                       [instr (Store source-expr (cast target-expr Register))])
                  (append-instruction! block instr))

                (append-bind! block (Identifier-name target) source-expr)
                #;(let* (#;[source-expr (translate-expr source block f-scope #f)])
                  (append-bind! (BasicBlock-block-env block) (Identifier-name target) source-expr)))
            
            (let* ([target-expr (translate-expr target block f-scope #t)]
               #;[source-expr (translate-expr source block f-scope #f)] 
               [instr (Store source-expr (cast target-expr Register))])
          (append-instruction! block instr)))

        #;(let* ([target-expr (translate-expr target block f-scope #t)]
               [source-expr (translate-expr source block f-scope #f)] 
               [instr (Store source-expr (cast target-expr Register))])
          (if (Identifier? target)
              (let* ([id-type (lookup-scope (Identifier-name target) f-scope)])
                (when (or (ty-Int? id-type) (ty-Bool? id-type))
                  (append-bind! (BasicBlock-block-env block) (Identifier-name target) source-expr)))
              (append-instruction! block instr)))
        (translate-statements r block func f-scope ret-block)]

       
       
       [(Print expr endl)
        ;; Handle print statement
        (let* ([expr-value (translate-expr expr block f-scope #f)]
               [newline (if endl #t #f)]
               [print-instr (PrintInstr expr-value newline)])
          (append-instruction! block print-instr))
        (set! register-count (+ register-count 1))
        (translate-statements r block func f-scope ret-block)]
       [(Conditional guard then else)
        ;; Handle conditional statement
        (let* ([cond-expr (translate-expr guard block f-scope #f)] 
               [then-block (create-new-bb)]
               [else-block (create-new-bb)]
               [cont-block (create-new-bb)]
               [br-instr (CondBr cond-expr
                                 (BasicBlock-label then-block)
                                 (BasicBlock-label else-block))])
          ;; add block to ht of all blocks
          (hash-set! all-blocks (BasicBlock-label then-block) then-block)
          (hash-set! all-blocks (BasicBlock-label else-block) else-block)
          (hash-set! all-blocks (BasicBlock-label cont-block) cont-block)
          
          (append-instruction! block br-instr)
       
          (set-BasicBlock-next/then! block then-block)
          (set-BasicBlock-else! block else-block)

          (set-BasicBlock-preds! then-block (append (BasicBlock-preds then-block) (list block)))
          (set-BasicBlock-preds! else-block (append (BasicBlock-preds else-block) (list block)))

          ;; Seal the then and else blocks
          (seal-block then-block)
          (seal-block else-block)

          (define lst-then (translate-statements (Block-stmts then) then-block func f-scope ret-block))
          
          
          (define lst-else (translate-statements (Block-stmts else) else-block func f-scope ret-block))
          ;;--------------------------

          (if (and (empty? (BasicBlock-instructions lst-then))
                   (not (string=? (BasicBlock-label lst-then)
                                        (format "retblock_~a" (Identifier-name (Func-id func))))))
              (begin
                (set-BasicBlock-preds! cont-block (append (BasicBlock-preds cont-block) (list lst-then)))
                (set-BasicBlock-next/then! lst-then cont-block)
                (append-instruction! lst-then (Br (BasicBlock-label cont-block))))
              (when (and (not (empty? (BasicBlock-instructions lst-then)))
                        (not (or (CondBr? (last (BasicBlock-instructions lst-then)))
                                 (Br? (last (BasicBlock-instructions lst-then)))))
                         (not (string=? (BasicBlock-label lst-then)
                                        (format "retblock_~a" (Identifier-name (Func-id func))))))
                (begin
                  (set-BasicBlock-preds! cont-block (append (BasicBlock-preds cont-block) (list lst-then)))
                  (set-BasicBlock-next/then! lst-then cont-block)
                  (append-instruction! lst-then (Br (BasicBlock-label cont-block)))))) 

          (if (and (empty? (BasicBlock-instructions lst-else))
                   (not (string=? (BasicBlock-label lst-else)
                                        (format "retblock_~a" (Identifier-name (Func-id func))))))
              (begin
                (set-BasicBlock-preds! cont-block (append (BasicBlock-preds cont-block) (list lst-else)))
                (set-BasicBlock-next/then! lst-else cont-block)
                (append-instruction! lst-else (Br (BasicBlock-label cont-block))))
              (when (and (not (empty? (BasicBlock-instructions lst-else)))
                        (not (or (CondBr? (last (BasicBlock-instructions lst-else)))
                                 (Br? (last (BasicBlock-instructions lst-else)))))
                        (not (string=? (BasicBlock-label lst-else)
                                        (format "retblock_~a" (Identifier-name (Func-id func))))))
                (begin
                  (set-BasicBlock-preds! cont-block (append (BasicBlock-preds cont-block) (list lst-else)))
                  (set-BasicBlock-next/then! lst-else cont-block)
                  (append-instruction! lst-else (Br (BasicBlock-label cont-block))))))
          ;;--------------------------
          ;; Seal the cont block
          (seal-block cont-block)
          
          (translate-statements r cont-block func f-scope ret-block))]
       [(Loop cond then)
        ;; Handle loop statement
        (let* ([loop-cond-block (create-new-bb)]
               [loop-body-block (create-new-bb)]
               [after-loop-block (create-new-bb)]
               [cond-expr (translate-expr cond loop-cond-block f-scope #f)]
               [br-instr (CondBr cond-expr
                                 (BasicBlock-label loop-body-block)
                                 (BasicBlock-label after-loop-block))])
          ;;add blocks to ht of all blocks
          (hash-set! all-blocks (BasicBlock-label loop-cond-block) loop-cond-block)
          (hash-set! all-blocks (BasicBlock-label loop-body-block) loop-body-block)
          (hash-set! all-blocks (BasicBlock-label after-loop-block) after-loop-block)

          
          (append-instruction! loop-cond-block br-instr)

          (append-instruction! block (Br (BasicBlock-label loop-cond-block)))
       
          (set-BasicBlock-next/then! block loop-cond-block)
          (set-BasicBlock-preds! loop-cond-block (append (BasicBlock-preds loop-cond-block) (list block)))

          

          (set-BasicBlock-next/then! loop-cond-block loop-body-block)
          (set-BasicBlock-preds! loop-body-block (append (BasicBlock-preds loop-body-block) (list loop-cond-block)))
          ;; Seal the loop body block now
          (seal-block loop-body-block)

          (define lst (translate-statements (Block-stmts then) loop-body-block func f-scope ret-block))

          (set-BasicBlock-preds! loop-cond-block (append (BasicBlock-preds loop-cond-block) (list lst)))

          ;; Seal the loop cond block now
          (seal-block loop-cond-block)
          
          (set-BasicBlock-next/then! lst loop-cond-block)
          ;; Loop back to condition
          (append-instruction! lst (Br (BasicBlock-label loop-cond-block)))

          (set-BasicBlock-else! loop-cond-block after-loop-block)
          (set-BasicBlock-preds! after-loop-block (append (BasicBlock-preds after-loop-block) (list loop-cond-block)))

          ;; Seal the after loop block now
          (seal-block after-loop-block)
          
          (translate-statements r after-loop-block func f-scope ret-block))]
       [(Ret ret-expr)
        ;; Handle return statement
        (let* ([ret-value (translate-expr ret-expr block f-scope #f)]
               [ret-ptr (lookup-scope (format "ret-var-~a" (Identifier-name (Func-id func))) f-scope)]
               [store-instr (Store ret-value ret-ptr)]
               [branch-instr (Br (format "retblock_~a" (Identifier-name (Func-id func))))])
          (if (string? ret-value)
              (if (string=? ret-value "void")
                  (begin
                    (hash-set! (BasicBlock-block-env block)
                               (format "ret-var-~a" (Identifier-name (Func-id func)))
                               "void")
                    (append-instruction! block branch-instr)
                    
                    (set-BasicBlock-next/then! block ret-block)
                    (set-BasicBlock-preds! ret-block (append (BasicBlock-preds ret-block) (list block)))
                    )
                  (if (string=? ret-value "null")
                      (begin
                        (hash-set! (BasicBlock-block-env block)
                                   (format "ret-var-~a" (Identifier-name (Func-id func)))
                                   (format "~a null" (llvm-type (Func-ret-type func))))
                        #;(append-instruction! block store-instr)
                    
                        (append-instruction! block branch-instr)
                        (set-BasicBlock-next/then! block ret-block)
                        (set-BasicBlock-preds! ret-block (append (BasicBlock-preds ret-block) (list block)))
                        )

                      (begin
                        (hash-set! (BasicBlock-block-env block)
                                   (format "ret-var-~a" (Identifier-name (Func-id func)))
                                   ret-value)
                        #;(append-instruction! block store-instr)
                    
                        (append-instruction! block branch-instr)
                        (set-BasicBlock-next/then! block ret-block)
                        (set-BasicBlock-preds! ret-block (append (BasicBlock-preds ret-block) (list block)))
                        )))
              (begin
                (hash-set! (BasicBlock-block-env block)
                           (format "ret-var-~a" (Identifier-name (Func-id func)))
                           ret-value)
                #;(append-instruction! block store-instr)
                (append-instruction! block branch-instr)
                (set-BasicBlock-next/then! block ret-block)
                (set-BasicBlock-preds! ret-block (append (BasicBlock-preds ret-block) (list block)))
                ))
          )
          ret-block]
       [(Invocation func-id args)
        ;; Process a function invocation
        (let* (
               [func-name (format "@~a" (Identifier-name func-id))]
               ;; list of argument expressions
               [arg-exprs args]  
               [arg-results (map (lambda ([arg : Expression]) (translate-expr arg block f-scope #f)) arg-exprs)]
               [return-type (llvm-type (fun-ret-type (Identifier-name func-id) (Program-functions top-prog)))]
               ;; Determine if a result register is needed
               [result (if (string=? return-type "void") "None" (make-register return-type))])  
          (define call-instr (CallInstr result func-name arg-results))  
          (append-instruction! block call-instr)
          #;(set! register-count (+ register-count 1))
          (translate-statements r block func f-scope ret-block))]
       [(Delete del) 
        (let* ([expr (cast (translate-expr del block f-scope #f) Register)]
               [casted-reg (make-register "i8*")]
               [cast-instr (Bitcast casted-reg expr "i8*")]
               [free-instr (FreeInstr casted-reg)])
          (append-instruction! block cast-instr)
          (append-instruction! block free-instr)
          (translate-statements r block func f-scope ret-block))]
       )]))

;;------------------------------------------------------XXX-------------------------------------------------------------



;;------------------------------------------------------CFG-Generation--------------------------------------------------
;; Struct sizes in bytes
(define struct-sizes (make-hash)) ;;("F". 16)

;; Build the global scope
(define global (build-func-scope (Program-declarations top-prog) #t))

;; Function to calculate the size of struct fields in bytes
(define (get-struct-size [struct-name : String] [fields-list : (Listof NestedField)])
  (define (size-helper [fields : (Listof NestedField)] [size : Natural]) : Natural
    (match fields
      ['() size]
      [(cons f r)
       (match (NestedField-type (cast f NestedField))
         [(ty-Int) (size-helper r (+ size 8))]
         [(ty-Bool) (size-helper r (+ size 8))]
         [(ty-Array) (size-helper r (+ size 8))]
         [(ty-Struct ty _)
           #;(define struct-size (cast (hash-ref struct-sizes (Identifier-name ty)) Natural))
           (size-helper r (+ size 8))]
         [else (error "Invalid type struct\n")])]))
  (define final-size (size-helper fields-list 0))
  (hash-set! struct-sizes struct-name final-size))

;;Function to process struct sizes
(define (process-struct-size [structs : (Listof ty-Struct)])
  (for-each (lambda ([struct : ty-Struct])
              (get-struct-size (Identifier-name (ty-Struct-id struct))
                               (cast (ty-Struct-fields struct) (Listof NestedField))))
            structs))

;; Function to process struct types
(define (process-type [struct : ty-Struct]) : TypeInstr
  (define (helper [fields : (Listof NestedField)] [typeinstr : TypeInstr]) : TypeInstr
    (match fields
      ['() typeinstr]
      [(cons f r)
       (define ty (llvm-type (NestedField-type (cast f NestedField))))
       (set-TypeInstr-sizes! typeinstr (append (TypeInstr-sizes typeinstr) (list ty)))
       (helper r typeinstr)]))
  (define type-name (format "%struct.~a" (Identifier-name (ty-Struct-id struct))))
  (define type-reg (Register (string->symbol type-name) (make-ptr type-name)))
  (define ty-instr (TypeInstr type-reg (list)))
  (helper (cast (ty-Struct-fields struct) (Listof NestedField)) ty-instr))

;; Funtion to process all types
(define (all-types [structs : (Listof ty-Struct)] [block : BasicBlock]) : Void
  (match structs
    ['() (void)]
    [(cons f r)
     (define instr (process-type f))
     (append-instruction! block instr)
     (all-types r block)]))

;; Functions to setup types blocl
(define (setup-types) : BasicBlock
  (define types-block (BasicBlock "typesblock" '() #f #f '() (make-hash) #f (make-hash)))  
  (process-struct-size (Program-types top-prog))
  (all-types (Program-types top-prog) types-block)
  types-block)

;; Function to write globals
(define (write-globals) : BasicBlock
  (define global-block (BasicBlock "globals" '() #f #f '() (make-hash) #f (make-hash))) 
  (for-each (lambda ([decl : Declaration])
              (append-instruction! global-block
                                   (GlobalInstr (Register
                                                 (string->symbol (format "@~a"
                                                 (Identifier-name (Declaration-id decl))))
                                                 (llvm-type (Declaration-type decl))))))
            (Program-declarations top-prog))
  global-block)

;; Function to make function headers
(define (make-args-header [args : (Listof Declaration)]) : String
  (define args-list
    (map (lambda ([arg : Declaration])
           (format "~a" (llvm-type (Declaration-type arg))))
         args))
  (string-join args-list ", "))

;; Function to load arguments
(define (store-args [args : (Listof Declaration)] [block : BasicBlock])
  (define counter 0)
  (for-each (lambda ([arg : Declaration])
              (append-instruction! block (Store (Register (string->symbol (format "%~a" (number->string counter)))
                                                         (format "~a" (llvm-type (Declaration-type arg))))
                                                (Register (string->symbol
                                                           (format "%~a" (Identifier-name (Declaration-id arg))))
                                                         (format "~a" (make-ptr (llvm-type (Declaration-type arg)))))))
              (set! counter (+ counter 1)))
            args))

;; Function to add binding to the block environment 
(define (append-bind! [block : BasicBlock] [key : String] [val : (U Register String)])
  (hash-set! (BasicBlock-block-env block) key val))

;; Function to add local params to block env
(define (add-local-env [block : BasicBlock] [params : (Listof Declaration)]) : Void
  (for-each (lambda ([decl : Declaration]) 
              (append-bind! block (Identifier-name (Declaration-id decl)) "Undefined"))
            params))



;; process single function
(define (process-function [fun : Func]) : FuncBlock
  (hash-clear! all-blocks)
  ;;(set! current-label-counter 0)
  (set! register-count (length (Func-parameters fun)))
  (define f-name (Identifier-name(Func-id fun)))
  (define params (Func-parameters fun))
  (define local-decls (Func-declarations fun))
  (define ret (Func-ret-type fun))

  ;;--------Types----------
  (define body-block (create-new-bb))
  (hash-set! all-blocks (BasicBlock-label body-block) body-block)
  ;; Seal this block
  (seal-block body-block)
  
  (define header (format "define ~a @~a(~a) {\n" (llvm-type ret) f-name (make-args-header (Func-parameters fun))))

  (define ret-val-ptr (Register (string->symbol (format "%ret-var-~a" f-name))
                    (make-ptr (llvm-type (Func-ret-type fun)))))

  (define ret-var (Binding
          (format "ret-var-~a" f-name)
          ret-val-ptr))

  (define local-scope (append (list ret-var) (build-func-scope local-decls #f) (build-func-scope params #f)))

  (define decls-block (BasicBlock (format "localdecls_~a" f-name) '() #f #f '() (make-hash) #f (make-hash))) 

  ;;(set-BasicBlock-next/then! decls-block body-block)
  
  (define func-scope (append local-scope global))
  ;;(translate-allocas local-scope body-block)

  ;; Function to write the local params to the Block env
  (add-local-env body-block local-decls)
  ;;--------------XX----------------
  

  ;; Store all %0,1,2... arguments into %arg1 like named registers
  #;(store-args params body-block)

  ;; add function params to block env
  (add-func-params body-block params)

  (define return-block (BasicBlock (format "retblock_~a" f-name) '() #f #f '() (make-hash) #f (make-hash)))
  (hash-set! all-blocks (BasicBlock-label return-block) return-block)
  
  ;;(process-statements (Func-statements fun) body-block fun func-scope)
  (define last-block (translate-statements (Func-statements fun) body-block fun func-scope return-block))

  (when (not (string=? (BasicBlock-label last-block) (format "retblock_~a" f-name)))
    (set-BasicBlock-next/then! last-block return-block)
    #;(when (not (empty? (BasicBlock-instructions last-block)))
      (set-BasicBlock-preds! return-block (append (BasicBlock-preds return-block) (list last-block))))
    (set-BasicBlock-preds! return-block (append (BasicBlock-preds return-block) (list last-block)))
    (append-instruction! last-block (Br (format "retblock_~a" f-name))))

  (when (and (empty? (BasicBlock-instructions last-block))
             (not #;(RetInstr? (last (BasicBlock-instructions last-block)))
                  (string=? (BasicBlock-label last-block) (format "retblock_~a" f-name))))
    (append-instruction! last-block (Br (format "retblock_~a" f-name)))
    )

  #;(if (not (ty-Void? ret))
         (let* ([ret-val (make-register (remove-ptr (Register-type ret-val-ptr)))]
                [load-instr (Load ret-val ret-val-ptr)]
                [ret-instr (RetInstr ret-val)])
           (append-instruction! return-block load-instr)
          
           (append-instruction! return-block ret-instr))
         (append-instruction! return-block (RetInstr "void")))
  ;; Seal the return block
  (seal-block return-block)

  (if (not (ty-Void? ret))
         (let* (#;[ret-val (make-register (remove-ptr (Register-type ret-val-ptr)))]
                [ret-val (find-id (format "ret-var-~a" f-name) return-block (remove-ptr (Register-type ret-val-ptr)))]
                #;[load-instr (Load ret-val ret-val-ptr)]
                [ret-instr (RetInstr ret-val)])
           (if (and (string? ret-val) (string-contains? ret-val "null"))
               (append-instruction! return-block (RetInstr (format "~a null" (llvm-type ret))))
               (append-instruction! return-block ret-instr)))
         (append-instruction! return-block (RetInstr "void")))

 
  (FuncBlock f-name ret params local-decls header body-block (hash-values all-blocks))) 

;; Process the AST
(define (process-ast [ast : Program]) : (Listof FuncBlock)
  (map process-function (Program-functions ast)))

(define types-block (setup-types))
(define global-block (write-globals))



(define CFG (process-ast top-prog))
;;CFG


;;------------------------------------------------------XXX-------------------------------------------------------------
(provide (all-defined-out))