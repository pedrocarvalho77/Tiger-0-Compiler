#!/bin/sh
# ok/*.tig       -> o parser tem de aceitar e produzir a AST em ok/*.expected
# fail/*.tig     -> o parser tem de rejeitar
# unsupported/*  -> usam strings/arrays (ainda não implementados): têm de falhar
# Com UPDATE=1 regenera os ficheiros .expected.
cd "$(dirname "$0")" || exit 1
bin=../tiger; failed=0

for f in ok/*.tig; do
  out=$($bin < "$f" 2>&1)
  if [ -n "$UPDATE" ]; then echo "$out" > "${f%.tig}.expected"; continue; fi
  if [ "$out" = "$(cat "${f%.tig}.expected")" ]; then echo "PASS $f"; else echo "FAIL $f"; failed=1; fi
done
for f in fail/*.tig unsupported/*.tig; do
  if $bin < "$f" >/dev/null 2>&1; then echo "FAIL $f (devia ser rejeitado)"; failed=1; else echo "PASS $f"; fi
done
exit $failed
