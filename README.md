# Tiger-0 Compiler

Front end for **Tiger-0**, a small subset of Tiger (Appel) used in the Compilers course (CC3001). It is written in Haskell with Alex (lexer) and Happy (parser).

* Lab work description: [trabalho.pdf](trabalho.pdf)
* Language description: [tiger0-reference.pdf](tiger0-reference.pdf) ([markdown version](tiger0-reference.md))

## Status

| Part | State |
|------|-------|
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
