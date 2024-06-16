# da-compiler - Compiler for MiniLanguage written in Typed/Racket

# Usage

# Typechecking and Parsing
- Typecheck parsed json -> `racket typecheck.rkt file-name.json`

# CFG Generation
- Write a dot file for CFG of a function (Stack-based) -> `racket write-dot.rkt file-name.json function-name`
- Write a dot file for CFG a function (SSA Form) -> `racket write-dot-ssa.rkt file-name.json function-name`
- Compile dot file to png -> `dot -Tpng dot-file.dot -o name.png`

# LLVM Code Generation
- Generate stack-based LLVM -> `racket stack-ll.rkt file-name.json`
- Generate SSA form LLVM (Without Optimizations) -> `racket ssa-ll.rkt file-name.json`
- Generate SSA form LLVM (With Optimizations) -> `racket ssa-ll.rkt file-name.json -O`
    - Optimiations - Sparse Simple Constant Progagation and Relaxed Dead Code Elimination

# Building Parser
To build and use the parser, ensure that the antlr and javax jar files are on
your classpath. You likely want to add something like the following to 
your ~/.bashrc file, assuming that you check out this repository in
$HOME.

```
BASE_DIR=$HOME

export CLASSPATH="$BASE_DIR/given_parser/antlr-4.12.0-complete.jar:$CLASSPATH"
export CLASSPATH="$BASE_DIR/given_parser/javax.json-1.0.4.jar:$CLASSPATH"
```

## Using the Parser

Now you should be able to use the built parser via

`java MiniCompiler <source.mini>`
