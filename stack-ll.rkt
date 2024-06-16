#lang typed/racket
(require racket/format)
(require racket/set)
(require compatibility/mlist)

(require "typecheck.rkt")
(require "cleaned-cfg-llvmir.rkt")

;;Function to write the function header
#;(define (write-function-header [func : FuncBlock])
  (define ))

   
(define print-form "@.print = private unnamed_addr constant [5 x i8] c\"%ld \\00\", align 1\n")
(define println-form "@.println = private unnamed_addr constant [5 x i8] c\"%ld\\0A\\00\", align 1\n")
(define read-form "@.read = private unnamed_addr constant [4 x i8] c\"%ld\\00\", align 1\n")
(define read-scratch-form "@.read_scratch = common global i64 0, align 8\n\n")

(struct Queue
  ([blocks : (Listof BasicBlock)])
  #:transparent
  #:mutable)

;;Helper function to insert icmp in Binary op instruction
(define (icmp? [sym : Symbol]) : String 
  (define str (symbol->string sym))
  (if (string-contains? str "icmp")
      (format "icmp ~a" (first (string-split str "-")))
      str))

(define (block-bfs [block : BasicBlock] [out : Output-Port]) : Void
  (define visited (make-hash))
  (define queue (Queue (list block))) ; Initialize queue with the initial block
   
  (define (enqueue [block : BasicBlock])
    (set-Queue-blocks! queue (append (Queue-blocks queue) (list block))))
  
  ; Process blocks in the queue until it's empty
  (let loop ()
    (cond
      [(empty? (Queue-blocks queue)) (void)]
      [else
       (define cur-block (first (Queue-blocks queue))) 
       
       (set-Queue-blocks! queue (rest (Queue-blocks queue)))
       
       
       ; Skip processing if the block has already been visited
       (when (hash-has-key? visited cur-block)
         (loop))

       (hash-set! visited cur-block #t)
       ; Write instructions for the current block
       (write-llvm-instructions cur-block out)

       
       ; Enqueue next/then and else blocks for processing
       (when (and (BasicBlock-next/then cur-block)
                  (false? (member (BasicBlock-next/then cur-block)
                                  (Queue-blocks queue)))
                  (not (hash-has-key? visited (BasicBlock-next/then cur-block))))
         (enqueue (cast (BasicBlock-next/then cur-block) BasicBlock)))
       (when (and (BasicBlock-else cur-block)
                  (false? (member (BasicBlock-else cur-block)
                                  (Queue-blocks queue)))
                  (not (hash-has-key? visited (BasicBlock-else cur-block))))
         (enqueue (cast (BasicBlock-else cur-block) BasicBlock)))
       (loop)])))

(define (write-llvm-instructions [block : BasicBlock] [out : Output-Port]) : Void
  ;;(define out (open-output-file file #:mode 'text #:exists 'replace))
  
  ; Function to write a single instruction to the file
  (define (write-instruction instr)
    (define instr-str
      (match instr
        [(Alloca (Register reg type) ty) (format "~a = alloca ~a" reg ty)]  
        [(Bitcast (Register result-reg type) (Register source-reg source-ty) cast-to)
         (define actual-source-ty (if (not (string-contains? source-ty "struct"))
                                      (make-ptr source-ty)
                                      source-ty))
         (format "~a = bitcast ~a ~a to ~a" (symbol->string result-reg) actual-source-ty source-reg cast-to)]  
        [(Store val (Register where-sym where-ty))
         (if (register? val)    
             (format "store ~a ~a, ~a ~a" (Register-type val) (Register-reg val) where-ty where-sym)
             (begin
               (if (or (string=? "true" (cast val String)) (string=? "false" (cast val String)))
                   (format "store i1 ~a, ~a ~a" (cast val String) where-ty where-sym)
                   (if (string=? "null" (cast val String))
                       (format "store ~a ~a, ~a ~a" (remove-ptr where-ty) (cast val String) where-ty where-sym)
                       (format "store i64 ~a, ~a ~a" (cast val String) where-ty where-sym))
                   )))]
        [(Load (Register res-sym res-ty) (Register ptr-sym ptr-ty))
         (format "~a = load ~a, ~a ~a" res-sym res-ty ptr-ty ptr-sym)]
        [(BinaryOp op (Register res-sym res-ty) op1 op2)
         (define op-str (icmp? op))
         (if (and (register? op1) (register? op2))
             (format "~a = ~a ~a ~a, ~a" res-sym op-str (Register-type op1) (Register-reg op1) (Register-reg op2))
             (begin
              (if (register? op1)
                  (format "~a = ~a ~a ~a, ~a" res-sym op-str (Register-type op1) (Register-reg op1) op2)
                  (if (register? op2)
                      (format "~a = ~a ~a ~a, ~a" res-sym op-str (Register-type op2) op1 (Register-reg op2))
                      (format "~a = ~a ~a ~a, ~a"
                              res-sym
                              op-str
                              (if (or (string=? op1 "true") (string=? op1 "false"))
                                  (format "i1")
                                  (format "i64"))
                              op1 op2)))))]
        [(RetInstr val)
         (if (register? val)
             (format "ret ~a ~a" (Register-type val) (Register-reg val))
             (begin
               (if (string=? "void" (cast val String)) 
                   "ret void"
                   (if (or (string=? "true" (cast val String)) (string=? "false" (cast val String)))
                       (format "ret i1 ~a" val)
                       (format "ret i64 ~a" val)))))]
        [(CondBr (Register cond-sym cond-ty) true-label false-label)
         (format "br i1 ~a, label %label~a, label %label~a" cond-sym true-label false-label)]
        [(Br lab)
         (format "br label %label~a" lab)] 
        [(GetElementPtrInstr (Register res-sym res-ty) (Register ptr-sym ptr-ty) index)
         (if (string=? ptr-ty "i64*")
             (if (register? index)
                 (format "~a = getelementptr ~a, ~a ~a, i64 ~a"
                         res-sym
                         (remove-ptr ptr-ty)
                         ptr-ty
                         ptr-sym
                         (Register-reg index))
                 (format "~a = getelementptr ~a, ~a ~a, i64 ~a"
                         res-sym
                         (remove-ptr ptr-ty)
                         ptr-ty
                         ptr-sym
                         index))
             (format "~a = getelementptr ~a, ~a ~a, i32 0, i32 ~a"
                     res-sym
                     (remove-ptr ptr-ty)
                     ptr-ty
                     ptr-sym
                     index))]
        [(CallInstr res func args) 
         (define f-args
           (map (lambda (arg)
                       (if (register? arg)
                           (format "~a ~a" (Register-type arg) (Register-reg arg))
                           (if (or (string=? "true" (cast arg String)) (string=? "false" (cast arg String)))
                               (format "i1 ~a" (cast arg String))
                               (if (string-contains? (cast arg String) "null")
                                   (cast arg String)
                                   (format "i64 ~a" (cast arg String)))))) args)) 
           (if (string? res)
               (format "call void ~a(~a)" func (string-join f-args ", "))
               (format "~a = call ~a ~a(~a)"
                       (Register-reg (cast res Register))
                       (Register-type (cast res Register))
                       func
                       (string-join f-args ", ")))]
        [(MallocInstr (Register sym ty) size)
         (format "~a = call i8* @malloc(i64 ~a)" (symbol->string sym) (number->string size))]
        [(TypeInstr (Register sym ty) sizes)
         (format "~a = type {~a}" (symbol->string sym) (string-join sizes ", "))]
        [(GlobalInstr (Register sym ty))
         (format "~a = common global ~a zeroinitializer" (symbol->string sym) ty)]
        [(PrintInstr exp endl)
         (if endl
             (format
              "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.println, i32 0, i32 0), i64 ~a)"
              (if (register? exp)
                  (Register-reg exp)
                  exp))
             (format
              "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.print, i32 0, i32 0), i64 ~a)"
              (if (register? exp)
                  (Register-reg exp)
                  exp)))] 
        [(ScanfInstr (Register sym ty))
         (format
          "call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.read, i32 0, i32 0), i64* @.read_scratch)")]
        [(FreeInstr (Register sym ty))
         (format
          "call void @free(i8* ~a)" sym)]))
    
    (fprintf out "~a\n" instr-str))
  
  ; Write the label of the basic block
  (when (not (or (string=? (BasicBlock-label block) "typesblock")
             (string=? (BasicBlock-label block) "globals")))
    (fprintf out "label~a:\n" (BasicBlock-label block)))
  
  ; Write each instruction of the block
  (for ([instr (BasicBlock-instructions block)])
    (write-instruction instr))
  
  (fprintf out "\n")
  
  ; Recursively process the next/then block
  #;(when (BasicBlock-next/then block)
    (write-llvm-instructions (cast (BasicBlock-next/then block) BasicBlock) out))
  
  ; Recursively process the else block
  #;(when (BasicBlock-else block)
    (write-llvm-instructions (cast (BasicBlock-else block) BasicBlock) out)))

(define lib-decls
  (string-append
   "declare i8* @malloc(i64)\n"
   "declare void @free(i8*)\n"
   "declare i32 @printf(i8*, ...)\n"
   "declare i32 @scanf(i8*, ...)\n"
   "@.println = private unnamed_addr constant [5 x i8] c\"%ld\\0A\\00\", align 1\n"
   "@.print = private unnamed_addr constant [5 x i8] c\"%ld \\00\", align 1\n"
   "@.read = private unnamed_addr constant [4 x i8] c\"%ld\\00\", align 1\n"
   "@.read_scratch = common global i64 0, align 8\n\n"))



(define (main-write-stack [file : String])
  
  (define out (open-output-file file #:mode 'text #:exists 'replace))
  (fprintf out lib-decls)

  (block-bfs types-block out)

  (block-bfs global-block out)
  
  ;;(fprintf out "\n")
  (for-each (lambda ([fblock : FuncBlock])
              (fprintf out (format "~a" (FuncBlock-header fblock)))
              (block-bfs (FuncBlock-body fblock) out)
              (fprintf out "}\n\n"))
            CFG)
  (close-output-port out))

(main-write-stack "output-stack.ll")

(provide (all-defined-out))
