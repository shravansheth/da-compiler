#lang typed/racket
(require racket/port)

(require "typecheck.rkt")
(require "ssa.rkt")

;; Function to find a FuncBlock by its label in a list of FuncBlocks
(define (find-func-block [cfg : (Listof FuncBlock)] [func-block-label : String]) : FuncBlock
  (define (find-func-helper [blocks : (Listof FuncBlock)] [label : String]) : FuncBlock
    (match blocks
      ['() (error 'find-func-block "Function block not found:" label)]
      [(cons first rest)
       (if (string=? (FuncBlock-label first) label)
           first
           (find-func-helper rest label))]))
  (find-func-helper cfg func-block-label))

;; Function to convert a statement to a string suitable for DOT labels
(define (Statement->string [stmt : LLVM-Instruction]) : String
  (format "~a\\n" stmt))

;; Function to convert block-env to a string suitable for DOT labels
(define (BlockEnv->string [block-env : BlockEnv]) : String
  (format "~a" block-env))

;; Helper function to write a single basic block and its edges, including predecessors
(define (write-dot-block [block : BasicBlock] [visited : (Listof String)] [port : Output-Port]) : (Listof String)
  (let ([block-label (BasicBlock-label block)])
    (unless (member block-label visited)
      (let ([instructions (map Statement->string (BasicBlock-instructions block))]
            [block-env (BlockEnv->string (BasicBlock-block-env block))])
        (displayln (format "  ~a [shape=box, label=\"{Block ~a\\n|~a|Preds: ~a|BlockEnv: ~a|}\"];" 
                           block-label block-label 
                           (apply string-append instructions)
                           (string-join (map BasicBlock-label (BasicBlock-preds block)) ", ")
                           block-env
                           
                           ) port))
      (set! visited (cons block-label visited))
      (let ([next-thn (BasicBlock-next/then block)])
        (when next-thn
          (displayln (format "  ~a -> ~a [label=\"next/then\"];" block-label (BasicBlock-label next-thn)) port)
          (set! visited (write-dot-block next-thn visited port))))
      (let ([else-blk (BasicBlock-else block)])
        (when else-blk
          (displayln (format "  ~a -> ~a [label=\"else\"];" block-label (BasicBlock-label else-blk)) port)
          (set! visited (write-dot-block else-blk visited port)))))
    visited))

;; Function to output a FuncBlock as a separate node with the header
(define (write-func-block [func_block : FuncBlock] [port : Output-Port])
  (let ([label (FuncBlock-label func_block)]
        [header (FuncBlock-header func_block)]) 
    (displayln (format "  ~a [shape=box, style=filled, color=lightgrey, label=\"Function ~a\\nHeader: ~a\\nReturn Type: ~a\\nParameters: ~a\\nLocals: ~a\"];" 
                       label label header
                       (Type->string (FuncBlock-ret-type func_block))
                       (string-join (map (lambda ([param : Declaration]) 
                                           (format "~a: ~a" (Identifier-name (Declaration-id param)) (Type->string (Declaration-type param))))
                                         (FuncBlock-parameters func_block)) "\\n")
                       (string-join (map (lambda ([local : Declaration]) 
                                           (format "~a: ~a" (Identifier-name (Declaration-id local)) (Type->string (Declaration-type local))))
                                         (FuncBlock-local func_block)) "\\n")) port)
    (when (FuncBlock-body func_block)
      (displayln (format "  ~a -> ~a [style=dashed];" label (BasicBlock-label (FuncBlock-body func_block))) port))))

;; Function to convert a Type to a string for display
(define (Type->string [ty : Type]) : String
  (match ty
    [(ty-Int) "int"]
    [(ty-Bool) "bool"]
    [else (format "~a" ty)]))

;; Main function to output the CFG to a DOT file
(define (output-cfg-to-dot [cfg : (Listof FuncBlock)] [function-name : String] [filepath : String]) : Void
  (define func-block (find-func-block cfg function-name))
  (define out (open-output-file filepath #:mode 'text #:exists 'replace))
  (write-string "digraph CFG {\n" out)
  (write-func-block func-block out)
  (write-dot-block (FuncBlock-body func-block) '() out)
  (write-string "}\n" out)
  (close-output-port out))

(output-cfg-to-dot CFG "biggestInList" "biggest.dot")
