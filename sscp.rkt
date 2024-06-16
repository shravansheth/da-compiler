#lang typed/racket
(require "typecheck.rkt")
(require "ssa.rkt")

(struct Critical ([instructions : (Listof LLVM-Instruction)])
  #:transparent
  #:mutable)

(struct Definitions ([defs : (HashTable Any LLVM-Instruction)])
  #:transparent
  #:mutable)

;;--------------------------------------------------SSCP---------------------------------------------------------
(define (string-to-boolean [str : String]) : Boolean
  (cond
    [(string=? str "true") #t]
    [(string=? str "false") #f]
    [else (error "Invalid boolean string")]))


(define (remove-instr [block : BasicBlock] [instr : LLVM-Instruction]) : Void
  (set-BasicBlock-instructions! block (remove instr (BasicBlock-instructions block))))

(define (prop-const [const-reg : Symbol] [const : String] [blocks : (Listof BasicBlock)])
  #;(printf (format "~a : ~a \n" const-reg const))
  (for-each (lambda ([block : BasicBlock])
              (for-each (lambda ([instr : LLVM-Instruction])
                          (match instr
                            [(Store value where)
                             (when (Register? value)
                               (when (eq? const-reg (Register-reg value))
                                 (set-Store-value! instr const)))]
                            [(BinaryOp _ (Register reg _) op1 op2)
                             (when (Register? op1)
                               (when (eq? const-reg (Register-reg op1))
                                 (set-BinaryOp-operand1! instr const)))
                             (when (Register? op2)
                               (when (eq? const-reg (Register-reg op2))
                                 (set-BinaryOp-operand2! instr const)))]
                            [(RetInstr val)
                             (when (Register? val)
                               (when (eq? const-reg (Register-reg val))
                                 (set-RetInstr-value! instr const)))]
                            [(CondBr cond _ _)
                             (when (Register? cond)
                               (when (eq? const-reg (Register-reg cond))
                                 (set-CondBr-condition! instr const)))]
                            [(GetElementPtrInstr r1 r2 r3)
                             (when (Register? r3)
                               (when (eq? const-reg (Register-reg r3))
                                 (set-GetElementPtrInstr-index! instr const)))]
                            [(CallInstr res _ args)

                             (for-each (lambda (arg)
                                         (when (Register? arg)
                                           (when (eq? const-reg (Register-reg arg))
                                             (let* ([res (index-of args arg)])
                                               (when res
                                                 (set-CallInstr-args! instr (list-set (CallInstr-args instr) res const)))))))
                                       args)]
                            [(PrintInstr val _)
                             (when (Register? val)
                               (when (eq? const-reg (Register-reg val))
                                 (set-PrintInstr-value! instr const)))]
                            [(Phi (Register reg _) values _)
                             (for-each (lambda (value)
                                         (when (Register? value)
                                           (when (eq? const-reg (Register-reg value))
                                             (let* ([res (index-of (Phi-values (cast instr Phi)) value)])
                                               (when res
                                                 #;(printf "this ran\n")
                                                 #;(printf "~a\n" res)
                                                 (set-Phi-values! instr
                                                                  (list-set
                                                                   (Phi-values instr)
                                                                   (cast res Integer)
                                                                   const))
                                                 #;(printf "~a\n" values))))))
                                       values)]
                            [_ (void)]))
                        (BasicBlock-instructions block)))  
            blocks)) 

(define (sscp [blocks : (Listof BasicBlock)]) : Boolean 
  (define changed 0)
  (define (helper [rec-blocks : (Listof BasicBlock)]) : Void
    (match rec-blocks
      ['() (void)]
      [(cons f r)
       (for-each (lambda ([instr : LLVM-Instruction])
                   (match instr                   
                     [(BinaryOp op (Register reg _) op1 op2)
                      (match op
                        ['and
                         (when (and (string? op1) (string? op2))
                           (prop-const reg (if (and (string-to-boolean op1) (string-to-boolean op2))
                                               "true"
                                               "false") blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['or
                         (when (and (string? op1) (string? op2))
                           (prop-const reg (if (or (string-to-boolean op1) (string-to-boolean op2))
                                               "true"
                                               "false") blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['eq-icmp
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (if (string=? op1 op2)
                                           "true"
                                           "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['ne-icmp
                         (when (and (string? op1) (string? op2))
                           (prop-const reg (if (not (string=? op1 op2))
                                               "true"
                                               "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['slt-icmp
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (if (<
                                            (cast (string->number op1) Real)
                                            (cast (string->number op2) Real))
                                           "true"
                                           "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['sgt-icmp
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (if (>
                                            (cast (string->number op1) Real)
                                            (cast (string->number op2) Real))
                                           "true"
                                           "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['sle-icmp
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (if (<=
                                            (cast (string->number op1) Real)
                                            (cast (string->number op2) Real))
                                           "true"
                                           "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['sge-icmp
                         (when (and (string? op1) (string? op2))
                           (prop-const reg (if (>=
                                                (cast (string->number op1) Real)
                                                (cast (string->number op2) Real))
                                               "true"
                                               "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['add
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (number->string (+
                                                        (cast (string->number op1) Real)
                                                        (cast (string->number op2) Real)))
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['sub
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (number->string (-
                                                        (cast (string->number op1) Real)
                                                        (cast (string->number op2) Real)))
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['mul
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (number->string (*
                                                        (cast (string->number op1) Real)
                                                        (cast (string->number op2) Real)))
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['sdiv
                         (when (and (string? op1) (string? op2))
                           (prop-const reg
                                       (number->string (quotient
                                                        (cast (string->number op1) Integer)
                                                        (cast (string->number op2) Integer)))
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        ['xor
                         (when (and (string? op1) (string=? (cast op2 String) "true"))
                           (prop-const reg (if (not (string-to-boolean op1))
                                               "true"
                                               "false")
                                       blocks)
                           (remove-instr f instr)
                           (set! changed 1))]
                        )]
                     [_ (void)])
                   )
                 (BasicBlock-instructions f))
       (helper r)
       ]))
  (helper blocks)
  (if (eq? changed 1)
      #t
      #f))
;;--------------------------------------------------XXXXXXXXXX---------------------------------------------------------



;;--------------------------------------------------Relaxed DCE---------------------------------------------------------
(define criticals (Critical '()))

#;(define (dce-mark [blocks : (Listof BasicBlock)]) : Boolean 
  (define changed 0)
  (define (helper [rec-blocks : (Listof BasicBlock)]) : Void
    (match rec-blocks
      ['() (void)]
      [(cons f r)
       (for-each (lambda ([instr : LLVM-Instruction])
                   (match instr
                     #;[(Alloca reg _)
                        (set-Critical-registers! criticals (append (list reg) (Critical-registers criticals)))]
                     #;[(Bitcast r1 r2 _)
                        (set-Critical-registers! criticals (append (list r1 r2) (Critical-registers criticals)))]
                     [(Load res ptr)
                      (set-Critical-registers! criticals (append (list res ptr) (Critical-registers criticals)))]
                     [(Store val ptr)
                      (when (Register? val)
                        (set-Critical-registers! criticals (append (list val) (Critical-registers criticals))))
                      (set-Critical-registers! criticals (append (list ptr) (Critical-registers criticals)))]
                     [(RetInstr val)
                      (when (Register? val)
                        (set-Critical-registers! criticals (append (list val) (Critical-registers criticals))))]
                     [(ScanfInstr reg)
                      (set-Critical-registers! criticals (append (list reg) (Critical-registers criticals)))]
                     [(PrintInstr val _)
                      (when (Register? val)
                        (set-Critical-registers! criticals (append (list val) (Critical-registers criticals))))]
                     [(CallInstr result _ args)
                      (when (Register? result)
                        (set-Critical-registers! criticals (append (list result) (Critical-registers criticals))))
                      (for-each (lambda (arg)
                                  (when (Register? arg)
                                    (set-Critical-registers!
                                     criticals
                                     (append (list arg) (Critical-registers criticals)))))
                                args)]
                     [_ (void)])
                   )
                 (BasicBlock-instructions f))
       (helper r)
       ]))
  (helper blocks)
  (if (eq? changed 1)
      #t
      #f))

#;(define (sweep [blocks : (Listof BasicBlock)]) : Void 
  (for-each (lambda ([block : BasicBlock])
              (for-each (lambda ([instr : LLVM-Instruction])
                          (match instr
                            #;[(Store value where)
                             (when (Register? value)
                               (when (eq? const-reg (Register-reg value))
                                 (set-Store-value! instr const)))]
                            [(Alloca res _)
                             (when (not (member res (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [(BinaryOp _ res op1 op2)
                             (when (not (member res (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [(Bitcast res src _)
                             (when (not (member res (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [(GetElementPtrInstr r1 r2 r3)
                             (when (not (member r1 (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [(MallocInstr res _)
                             (when (not (member res (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [(FreeInstr res)
                             (when (not (member res (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [(Phi res values _)
                             (when (not (member res (Critical-registers criticals)))
                               (remove-instr block instr))]
                            [_ (void)]))
                        (BasicBlock-instructions block)))
            blocks))

;; Function to check if instruction is LLVM-Instruction
(define (LLVM-Instruction? [instr : Any]) : Boolean
  (or (Alloca? instr)
      (Bitcast? instr)
      (Store? instr)
      (Load? instr)
      (BinaryOp? instr)
      (RetInstr? instr)
      (CondBr? instr)
      (Br? instr)
      (GetElementPtrInstr? instr)
      (CallInstr? instr)
      (PrintInstr? instr)
      (ScanfInstr? instr)
      (MallocInstr? instr)
      (TypeInstr? instr)
      (GlobalInstr? instr)
      (FreeInstr? instr)
      (Phi? instr)))


(define all-defs (Definitions (make-hash)))

;; Function to get reaching definition for each register
(define (reaching-defs [blocks : (Listof BasicBlock)]) : Void 
  (for-each (lambda ([block : BasicBlock])
              (for-each (lambda ([instr : LLVM-Instruction])
                          (match instr
                            [(Alloca res _)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [(Bitcast res src _)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [(Load res ptr)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [(BinaryOp _ res op1 op2)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [(GetElementPtrInstr r1 r2 r3)
                             (hash-set! (Definitions-defs all-defs) (Register-reg r1) instr)]
                            [(CallInstr res _ _)
                             (when (Register? res)
                               (hash-set! (Definitions-defs all-defs) (Register-reg res) instr))]
                            [(ScanfInstr res)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [(MallocInstr res _)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [(Phi res values _)
                             (hash-set! (Definitions-defs all-defs) (Register-reg res) instr)]
                            [_ (void)]))
                        (BasicBlock-instructions block)))
            blocks))


;; Helper function to recursively mark instructions critical
(define (recursive-mark [instr : LLVM-Instruction]) : Void
  (when (not (member instr (Critical-instructions criticals)))
    (set-Critical-instructions! criticals (append (Critical-instructions criticals) (list instr))) 
    (match instr
      [(Alloca res _)
       (let* ([defn (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)])
         (when (and (LLVM-Instruction? defn)
                    (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(Bitcast res src _)
       (let* ([defn1 (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)]
              [defn2 (hash-ref (Definitions-defs all-defs) (Register-reg src) #f)])
         (when (and (LLVM-Instruction? defn1)
                    (not (member defn1 (Critical-instructions criticals))))
           (recursive-mark (cast defn1 LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn2)
                    (not (member defn2 (Critical-instructions criticals))))
           (recursive-mark (cast defn2 LLVM-Instruction))))]
      [(Store val where)
       (let* ([defn1 (if (Register? val)
                         (hash-ref (Definitions-defs all-defs) (Register-reg val) #f)
                         #f)]
              [defn2 (hash-ref (Definitions-defs all-defs) (Register-reg where) #f)])
         (when (and (LLVM-Instruction? defn1)
                (not (member defn1 (Critical-instructions criticals))))
           (recursive-mark (cast defn1 LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn2)
                    (not (member defn2 (Critical-instructions criticals))))
           (recursive-mark (cast defn2 LLVM-Instruction))))]
      [(Load res ptr)
       #;(printf "~a\n" instr)
       (let* ([defn1 (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)]
              [defn2 (hash-ref (Definitions-defs all-defs) (Register-reg ptr) #f)])
         (when (and (LLVM-Instruction? defn1)
                    (not (member defn1 (Critical-instructions criticals))))
           #;(printf "this ran\n")
           (recursive-mark (cast defn1 LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn2)
                    (not (member defn2 (Critical-instructions criticals))))
           (recursive-mark (cast defn2 LLVM-Instruction))))]
      [(BinaryOp _ res op1 op2)
       (let* ([defn-res (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)]
              [defn-op1 (if (Register? op1)
                         (hash-ref (Definitions-defs all-defs) (Register-reg op1) #f)
                         #f)]
              [defn-op2 (if (Register? op2)
                         (hash-ref (Definitions-defs all-defs) (Register-reg op2) #f)
                         #f)])
         (when (and (LLVM-Instruction? defn-res)
                    (not (member defn-res (Critical-instructions criticals))))
           (recursive-mark (cast defn-res LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn-op1)
                    (not (member defn-op1 (Critical-instructions criticals))))
           (recursive-mark (cast defn-op1 LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn-op2)
                (not (member defn-op2 (Critical-instructions criticals))))
           (recursive-mark (cast defn-op2 LLVM-Instruction))))]
      [(RetInstr val)
       (let* ([defn (if (Register? val)
                        (hash-ref (Definitions-defs all-defs) (Register-reg val) #f)
                        #f)])
         (when (and (LLVM-Instruction? defn)
                    (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(CondBr cond _ _)
       (let* ([defn (if (Register? cond)
                        (hash-ref (Definitions-defs all-defs) (Register-reg cond) #f)
                        #f)])
         (when (and (LLVM-Instruction? defn)
                    (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(GetElementPtrInstr res ptr ind)
       (let* ([defn-res (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)]
              [defn-ptr (hash-ref (Definitions-defs all-defs) (Register-reg ptr) #f)]
              [defn-ind (if (Register? ind)
                            (hash-ref (Definitions-defs all-defs) (Register-reg ind) #f)
                            #f)])
         (when (and (LLVM-Instruction? defn-res)
                    (not (member defn-res (Critical-instructions criticals))))
           (recursive-mark (cast defn-res LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn-ptr)
                    (not (member defn-ptr (Critical-instructions criticals))))
           (recursive-mark (cast defn-ptr LLVM-Instruction)))
         (when (and (LLVM-Instruction? defn-ind)
                (not (member defn-ind (Critical-instructions criticals))))
           (recursive-mark (cast defn-ind LLVM-Instruction))))]
      [(CallInstr res _ args)
       (let* ([defn-res (if (Register? res)
                            (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)
                            #f)])
         (when (and (LLVM-Instruction? defn-res)
                    (not (member defn-res (Critical-instructions criticals))))
           (recursive-mark (cast defn-res LLVM-Instruction))))
       (for-each (lambda ([arg : (U Register String)])
                   (let* ([defn-arg (if (Register? arg)
                                        (hash-ref (Definitions-defs all-defs) (Register-reg arg) #f)
                                        #f)])
                     (when (and (LLVM-Instruction? defn-arg)
                                (not (member defn-arg (Critical-instructions criticals))))
                       (recursive-mark (cast defn-arg LLVM-Instruction)))))
                 args)]
      [(PrintInstr val _)
       (let* ([defn (if (Register? val)
                        (hash-ref (Definitions-defs all-defs) (Register-reg val) #f)
                        #f)])
         (when (and (LLVM-Instruction? defn)
                    (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(ScanfInstr res)
       (let* ([defn (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)])
         (when (and (LLVM-Instruction? defn)
                    (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(MallocInstr res _)
       (let* ([defn (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)])
         (when (not (member defn (Critical-instructions criticals)))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(FreeInstr res)
       (let* ([defn (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)])
         (when (and (LLVM-Instruction? defn)
                (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))]
      [(Phi res vals _)
       (let* ([defn (hash-ref (Definitions-defs all-defs) (Register-reg res) #f)])
         (when (and (LLVM-Instruction? defn)
                    (not (member defn (Critical-instructions criticals))))
           (recursive-mark (cast defn LLVM-Instruction))))
       (for-each (lambda ([val : (U Register String)])
                   (let* ([defn-val (if (Register? val)
                                        (hash-ref (Definitions-defs all-defs) (Register-reg val) #f)
                                        #f)])
                     (when (and (LLVM-Instruction? defn-val)
                                (not (member defn-val (Critical-instructions criticals))))
                       (recursive-mark (cast defn-val LLVM-Instruction)))))
                 vals)]
      [_ (void)])))


;; Function to go through instruction and mark instructions critical
(define (main-mark [blocks : (Listof BasicBlock)]) : Void
  (for-each (lambda ([block : BasicBlock])
              (for-each (lambda ([instr : LLVM-Instruction])
                          (when (or
                                 (Load? instr)
                                 (Store? instr)
                                 (RetInstr? instr)
                                 (ScanfInstr? instr)
                                 (PrintInstr? instr)
                                 (CallInstr? instr)
                                 (CondBr? instr)
                                 (Br? instr))
                              #;(set-Critical-instructions! (append (Critical-instructions) (list instr)))
                              (recursive-mark instr)))
                        (BasicBlock-instructions block)))
            blocks)) 

;; Function to sweep dead instructions
(define (sweep [blocks : (Listof BasicBlock)]) : Void
  (for-each (lambda ([block : BasicBlock])
              (for-each (lambda ([instr : LLVM-Instruction])
                          (when (not (member instr (Critical-instructions criticals)))
                            (remove-instr block instr)))
                        (BasicBlock-instructions block)))
            blocks))
;;--------------------------------------------------XXXXXXXXXX---------------------------------------------------------

(define (main-sscp [cfg : (Listof FuncBlock)]) : Void
  (define change (map (lambda ([func : FuncBlock])
                        (if (sscp (FuncBlock-blocks func))
                            1
                            0))
                      cfg))
  (if (member 1 change)
      (main-sscp cfg)
      (void)))

(define (main-dce [cfg : (Listof FuncBlock)]) : Void
  (for-each (lambda ([func : FuncBlock])
              (hash-clear! (Definitions-defs all-defs))
              (set-Critical-instructions! criticals '())
              (reaching-defs (FuncBlock-blocks func))
              (main-mark (FuncBlock-blocks func))
              (sweep (FuncBlock-blocks func)))
            cfg))


(define (main-opt [cfg : (Listof FuncBlock)])
  (main-sscp cfg)
  (main-dce cfg))

(main-opt CFG)

(provide (all-defined-out))