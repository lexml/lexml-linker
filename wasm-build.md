# Building LexML Linker for WebAssembly

This project has a Cabal executable named `lexml-linker-wasm` that is buildable
only for the `wasm32-wasi` target. It uses GHC's WebAssembly JavaScript FFI and
is linked as a WASI reactor module, then post-processed into browser-side
JavaScript by GHC's `post-link.mjs` script.

The relevant project files are:

- `lexml-parser.cabal`: declares `exe:lexml-linker-wasm`.
- `src/wasm/haskell/LinkerWasm.hs`: exports the JavaScript-callable
  `lexml_link` function.
- `wasm-demo/build.sh` and `wasm-rich-demo/build.sh`: copy the built `.wasm`
  file and run GHC's JSFFI post-link step.

## Install system prerequisites

On Debian or Ubuntu:

```bash
sudo apt-get update
sudo apt-get install -y curl jq unzip zstd nodejs npm python3
```

Why these tools are needed:

- `curl`, `jq`, `unzip`, and `zstd` are used by the `ghc-wasm-meta` installer.
- `node` is used by GHC's `post-link.mjs` JSFFI post-linker.
- `python3` is only needed to serve the browser demos locally.

Optional, but useful for command-line testing of non-browser WASI modules:

```bash
sudo apt-get install -y wasmtime
```

## Install the WASM GHC toolchain

GHC cannot emit WebAssembly from a normal host-native GHC installation. You need
a separate cross compiler whose target is `wasm32-wasi`.

The simplest setup is the `ghc-wasm-meta` bootstrap installer:

```bash
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | sh
```

Then load the toolchain environment in each shell where you build the project:

```bash
source ~/.ghc-wasm/env
```

This puts tools such as these on `PATH`:

- `wasm32-wasi-ghc`
- `wasm32-wasi-ghc-pkg`
- `wasm32-wasi-cabal`
- `wasm32-wasi-hsc2hs`

Check that the installed compiler is the expected target compiler:

```bash
wasm32-wasi-ghc --version
wasm32-wasi-ghc --print-target-platform
wasm32-wasi-cabal --version
node --version
```

This project's WASM executable currently depends on `base >=4.22 && <4.23` and
`ghc-experimental ==9.1401.*`, so use the GHC 9.14 WASM toolchain. The
`ghc-wasm-meta` default flavour is currently `9.14`, but you can make that
explicit when bootstrapping:

```bash
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.14 sh
source ~/.ghc-wasm/env
```

## Install host-native Haskell build tools

The package uses Alex to generate `LexML.Linker.Lexer` from
`src/main/haskell/LexML/Linker/Lexer.x`. Cabal's `build-tool-depends` should
build the tool when needed, but the existing demo instructions expect a
host-native `alex` executable on `PATH`.

Use a normal host-native Cabal, not `wasm32-wasi-cabal`, to install it:

```bash
cabal update
cabal install alex
```

If `~/.cabal/bin` is not already on `PATH`, add it before building:

```bash
export PATH="$HOME/.cabal/bin:$PATH"
```

Verify:

```bash
alex --version
```

## Build the WASM executable

From the repository root:

```bash
source ~/.ghc-wasm/env
export PATH="$HOME/.cabal/bin:$PATH"
wasm32-wasi-cabal update
wasm32-wasi-cabal build exe:lexml-linker-wasm
```

Find the generated `.wasm` file:

```bash
wasm32-wasi-cabal list-bin exe:lexml-linker-wasm
```

## Generate browser demo artifacts

The raw `.wasm` file is not enough for this project's browser demos because the
executable uses GHC's JavaScript FFI. Run the demo build script to copy the
module and generate the matching `lexml-linker.mjs` file:

```bash
bash wasm-demo/build.sh
```

That creates:

- `wasm-demo/lexml-linker.wasm`
- `wasm-demo/lexml-linker.mjs`

For the rich-text demo:

```bash
bash wasm-rich-demo/build.sh
```

That creates:

- `wasm-rich-demo/lexml-linker.wasm`
- `wasm-rich-demo/lexml-linker.mjs`

## Run the browser demos

For the basic demo, serve the repository root over HTTP:

```bash
python3 -m http.server 8000
```

Open:

```text
http://localhost:8000/wasm-demo/
```

For the rich-text demo, use its proxy server:

```bash
python3 wasm-rich-demo/server.py
```

Open:

```text
http://localhost:8000/wasm-rich-demo/
```

The browser demos load some JavaScript dependencies from CDNs, including the
WASI browser shim, so the page needs internet access at runtime.

## Troubleshooting

If `wasm32-wasi-cabal` is not found, run:

```bash
source ~/.ghc-wasm/env
```

If Cabal rejects `base` or `ghc-experimental` versions, check that you are using
the GHC 9.14 WASM toolchain:

```bash
wasm32-wasi-ghc --numeric-version
wasm32-wasi-ghc-pkg list ghc-experimental
```

If the demo build script fails with `node: command not found`, install Node.js
and rerun the script:

```bash
sudo apt-get install -y nodejs npm
bash wasm-demo/build.sh
```

If loading the demo from the browser fails from a `file://` URL, serve it over
HTTP instead. ES modules and WebAssembly are browser-restricted when opened
directly from the filesystem.

## References

- GHC User's Guide, "Using the GHC WebAssembly backend":
  https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html
- `ghc-wasm-meta` installer and toolchain documentation:
  https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
