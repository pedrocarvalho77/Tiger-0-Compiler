#!/bin/sh
# ok/*.tig    -> tem de fazer parse (AST igual a ok/*.expected) e, ao correr o codigo
#                intermedio, dar o output de ok/*.out (stdin: ok/*.in, se existir)
# fail/*.tig  -> o compilador tem de rejeitar (erro de sintaxe ou de tipos)
# Se MARS_JAR apontar para o simulador MARS, o MIPS gerado tambem e executado e
# comparado com ok/*.out.
# Com UPDATE=1 regenera os .expected (os .out escrevem-se a mao).
cd "$(dirname "$0")" || exit 1
failed=0
check() { # nome esperado obtido
  if [ "$2" = "$3" ]; then echo "PASS $1"; else echo "FAIL $1"; failed=1; fi
}

for f in ok/*.tig; do
  b=${f%.tig}; in=/dev/null; [ -f "$b.in" ] && in=$b.in
  ast=$(../tiger --ast < "$f" 2>&1)
  if [ -n "$UPDATE" ]; then printf '%s\n' "$ast" > "$b.expected"; continue; fi
  if ../tiger < "$f" >/dev/null 2>&1; then echo "PASS $f (tipos)"; else echo "FAIL $f (tipos)"; failed=1; fi
  check "$f (ast)" "$(cat "$b.expected")" "$ast"
  check "$f (ir)" "$(cat "$b.out")" "$(../build/irrun "$f" < "$in")"
  if [ -n "$MARS_JAR" ]; then
    ../tiger < "$f" > ../build/test.s
    check "$f (mips)" "$(cat "$b.out")" "$(java -jar "$MARS_JAR" nc ../build/test.s < "$in" 2>&1 | grep -v '^MARS\|^Copyright')"
  fi
done
for f in fail/*.tig; do
  if ../tiger < "$f" >/dev/null 2>&1; then echo "FAIL $f (devia ser rejeitado)"; failed=1; else echo "PASS $f"; fi
done
exit $failed
