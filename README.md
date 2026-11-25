# Tiger-0 Compiler

Front end for **Tiger-0**, a small subset of Tiger (Appel) used in the Compilers course (CC3001). It is written in Haskell with Alex (lexer) and Happy (parser).

* Lab work description: [trabalho.pdf](trabalho.pdf)
* Language description: [tiger0-reference.pdf](tiger0-reference.pdf) ([markdown version](tiger0-reference.md))

## What it does

Reads a Tiger-0 program from stdin and writes MIPS assembly (SPIM/MARS) to stdout.

| Stage | File |
|-------|------|
| Lexer | `src/Lexer.x` |
| Parser (AST) | `src/Parser.y` (no shift/reduce conflicts; precedences as in the spec) |
| Type checker | `src/TypeCheck.hs` |
| Intermediate code (three-address) | `src/Interm.hs` |
| MIPS generation | `src/TigerMips.hs` |

Supported: integers, strings (only for `print`), `intArray`, variables, functions, `if`, `while`, `for`, `break`, `let`, `not`, `print`, `printi`, `scani`. Top-level `var`s are globals; functions see them.

Type and scope errors (wrong operand or argument types, wrong argument count, undeclared names, `break` outside a loop, assigning to a `for` variable, ...) are reported on stderr before any code is generated; error messages have no source positions.

Limitations: functions take at most 4 arguments, strings cannot be compared (rejected by the type checker) or stored in arrays, and there are no array bounds or division-by-zero checks.

## Requirements

`ghc` (with the `mtl` and `containers` packages), `alex` and `happy`. To run the generated assembly, a MIPS simulator such as [MARS](https://github.com/dpetersanderson/MARS) or SPIM.

## Build and run

```sh
make
./tiger < test/ok/factorial.tig > factorial.s     # MIPS assembly
java -jar Mars4_5.jar nc factorial.s              # prints 3628800
./tiger --ir  < test/ok/factorial.tig             # intermediate code
./tiger --ast < test/ok/factorial.tig             # abstract syntax tree
```

## Tests

```sh
make test
MARS_JAR=/path/to/Mars4_5.jar make test    # also runs the generated MIPS
```

* `test/ok/` programs that must parse (AST compared with `.expected`) and print the output in `.out` (stdin from `.in`). The output is checked by running the intermediate code with the small interpreter in `test/IRRun.hs`, and by MARS when `MARS_JAR` is set.
* `test/fail/` programs that must be rejected (syntax or type errors).
* Regenerate the AST files with `UPDATE=1 test/run.sh`.

------|-------|
| Lexer (`src/Lexer.x`) | done (no string literals yet) |
| Parser (`src/Parser.y`) | done for integers, variables, functions, `if`, `while`, `for`, `let`, `printi`, `scani`. Strings, `print` and arrays (`intArray [n] of v`, `a[i]`) are **not** supported yet |
| Intermediate code (`src/Interm.hs`) | work in progress, only type-checked, not wired into `Main` |
| MIPS output (`src/TigerMips.hs`) | work in progress, not wired into `Main` |

`Main` currently reads a program from stdin and prints its abstract syntax tree.

## Requirements

`ghc` (with the `mtl` package), `alex` and `happy`.

## Build and run

```sh
make
./tiger < test/ok/factorial.tig
```

```
LetIn [FunDecl (FunDefType "fact" [TypeField "n" TypeInt] TypeInt (IfThenElse ...))] [PrintInt (FunCall "fact" [Num 10])]
```

## Tests

```sh
make test
```

* `test/ok/` programs that must parse; the AST is compared with the `.expected` file (regenerate with `UPDATE=1 test/run.sh`).
* `test/fail/` programs that must be rejected.
* `test/unsupported/` the spec examples that use strings or arrays; they are expected to fail until those features are implemented.

----

Pedro Carvalho, 2021
