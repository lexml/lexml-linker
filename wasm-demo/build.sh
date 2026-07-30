#!/usr/bin/env bash

set -euo pipefail

demo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
wasm_file="$(wasm32-wasi-cabal list-bin exe:lexml-linker-wasm)"
ghc_libdir="$(wasm32-wasi-ghc --print-libdir)"

cp -- "$wasm_file" "$demo_dir/lexml-linker.wasm"
node "$ghc_libdir/post-link.mjs" \
  -i "$demo_dir/lexml-linker.wasm" \
  -o "$demo_dir/lexml-linker.mjs"

echo "Browser files generated in $demo_dir"
