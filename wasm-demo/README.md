# LexML Linker browser demo

Build the WASM executable and generate its browser-side JSFFI module:

```bash
source ~/.ghc-wasm/env
wasm32-wasi-cabal build exe:lexml-linker-wasm
bash wasm-demo/build.sh
```

The build requires a host-native `alex` executable on `PATH`.

Serve the repository over HTTP (ES modules and WASM cannot reliably be loaded
from a `file://` URL):

```bash
python3 -m http.server 8000
```

Then open <http://localhost:8000/wasm-demo/>.

The WASI browser shim is loaded from `esm.sh`, so the demo requires internet
access when the page is loaded.
