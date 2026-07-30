# LexML Linker rich-text browser demo

This demo uses Quill 2 and enriches the editor contents after 500 ms without a
user change. A revision counter prevents an older asynchronous linker result
from replacing newer edits.

Build the WASM executable and generate its browser-side JSFFI module:

```bash
source ~/.ghc-wasm/env
wasm32-wasi-cabal build exe:lexml-linker-wasm
bash wasm-rich-demo/build.sh
```

The build requires a host-native `alex` executable on `PATH`.

Start the demo server:

```bash
python3 wasm-rich-demo/server.py
```

Then open <http://localhost:8000/wasm-rich-demo/>.

The LexML resolver does not allow its HTML pages to be read directly by a
cross-origin browser request. The demo server therefore exposes a restricted
same-origin `/lexml-metadata` endpoint that accepts only document-level
`urn:lex:` values and proxies the corresponding LexML page.

Quill and the WASI browser shim are loaded from CDNs, so the demo requires
internet access when the page is loaded.
