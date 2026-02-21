# WebAssembly support in Swift

WebAssembly is a platform that significantly differs from hardware platforms that Swift already supports.
While it's a virtual machine, there are considerations to be taken into account when targeting it:

* WebAssembly is still at an early stage, so many features you'd be expect from other platforms are not
available yet, specifically:
  1. `wasm64` variant is not specified yet, only the 32-bit `wasm32` variant is supported in WebAssembly
  hosts such as browsers.
  2. While a preview of multi-threading and atomics is available in some browsers and stand-alone
  WebAssembly hosts, [the corresponding proposal](https://github.com/WebAssembly/threads/) haven't
  formally reached the implementation phase yet.
    The multi-threading feature is available in `wasm32-unknown-wasip1-threads` target, but it's not
    in `wasm32-unknown-wasi` target.
  3. Dynamic linking is not formally specified and tooling for it is not available yet.
* Binary size is a high priority requirement. Since WebAssembly payloads are usually served in browsers,
one wouldn't want end users to download multi-megabyte binaries.

## Running Wasm stdlib tests

If you're a compiler/stdlib engineer, this invocation builds LLVM, Swift, installs those together with necessary
tools, and also builds WasmKit to execute stdlib tests:

```
./utils/build-script --build-wasm-stdlib --wasmkit --build-embedded-stdlib --build-embedded-stdlib-cross-compiling \
  --install-swift --install-llvm \
  '--llvm-install-components=llvm-ar;llvm-nm;llvm-ranlib;llvm-cov;llvm-profdata;llvm-objdump;llvm-objcopy;llvm-symbolizer;IndexStore;clang;clang-resource-headers;builtins;runtimes;clangd;libclang;dsymutil;LTO;clang-features-file;lld' \
  --sccache
```

On macOS it's also best to avoid cross-compiling host tools from arm64 to x86 and vice versa, add this option to avoid that: `--infer-cross-compile-hosts-on-darwin=false`.

If you need to only run compiler/stdlib tests after building everything with the `build-script` command listed above, switch to the build directory first:

```
cd build/Ninja-RelWithDebInfoAssert
```

Then run the compiler and stdlib test suite via `ninja` (assuming your host is arm64 macOS, adjust paths accordingly otherwise):

```
PATH="$(pwd)/wasmkit-macosx-arm64/bin:$(pwd)/llvm-macosx-arm64/bin:$PATH" \
  ninja check-swift-wasi-wasm32-custom check-swift-embedded-wasi -C wasmstdlib-macosx-arm64
```

Filter to a subset of tests you'd like to run with `LIT_FILTER`. Here's an example to run only tests with `embedded` in their file path:

```
LIT_FILTER='(embedded)' PATH="$(pwd)/wasmkit-macosx-arm64/bin:$(pwd)/llvm-macosx-arm64/bin:$PATH" \
  ninja check-swift-wasi-wasm32-custom check-swift-embedded-wasi -C wasmstdlib-macosx-arm64
```

Filter out tests you don't want to run with `LIT_FILTER_OUT`. Here's an example that excludes tests containing `KeyPath` in their file path:

```
LIT_FILTER_OUT='(KeyPath)' PATH="$(pwd)/wasmkit-macosx-arm64/bin:$(pwd)/llvm-macosx-arm64/bin:$PATH" \
  ninja check-swift-wasi-wasm32-custom check-swift-embedded-wasi -C wasmstdlib-macosx-arm64
```

Values passed to these environment variables are regular expressions, thus to exclude both `embedded` and `KeyPath`
use `|` regex operator:

```
LIT_FILTER_OUT='(embedded|KeyPath)' PATH="$(pwd)/wasmkit-macosx-arm64/bin:$(pwd)/llvm-macosx-arm64/bin:$PATH" \
  ninja check-swift-wasi-wasm32-custom check-swift-embedded-wasi -C wasmstdlib-macosx-arm64
```


## Building Swift SDK for WebAssembly

The [Swift SDK](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0387-cross-compilation-destinations.md)
for WebAssembly is built and tested using the following command (exclude `--sccache` if it's not installed or build caching is not needed):

```bash
./utils/build-script --sccache --build-wasm-stdlib --wasmkit --install-llvm --install-swift --swiftpm --install-swiftpm \
  --llbuild --install-llbuild --swift-testing --install-swift-testing \
  --swift-testing-macros --install-swift-testing-macros --build-embedded-stdlib --build-embedded-stdlib-cross-compiling \
  '--llvm-install-components=llvm-ar;llvm-nm;llvm-ranlib;llvm-cov;llvm-profdata;llvm-objdump;llvm-objcopy;llvm-symbolizer;IndexStore;clang;clang-resource-headers;builtins;runtimes;clangd;libclang;dsymutil;LTO;clang-features-file;lld'
```

This command will build the Swift compiler for the host platform and then build the Swift standard library
for WebAssembly targets. Toolchain `lit.py` tests will run on the freshly built stdlib, both for embedded and non-embedded
builds.

The resulting Swift SDK `.artifactbundle` will be placed in the `../swift-sdk-generator/Bundles`
directory. Install it using the following command (assuming you're in the `swift` directory of `update-checkout` clones):

```
swift sdk install ../swift-sdk-generator/Bundles/swift-DEVELOPMENT-SNAPSHOT_wasm.artifactbundle
```

## Building Swift SDK for WebAssembly without building the compiler

Building the Swift compiler is a time-consuming process. If you only want to build the Swift standard library
with pre-built Swift compiler, you can use the following command:

```console
$ SWIFT_TOOLS_PATH=path/to/swift-development-snapshot/usr/bin
$ ./utils/build-script \
    --skip-build-llvm \
    --skip-build-swift \
    --skip-build-cmark \
    --build-wasm-stdlib \
    --native-swift-tools-path="$SWIFT_TOOLS_PATH" \
    --native-clang-tools-path="$SWIFT_TOOLS_PATH" \
    --native-llvm-tools-path="$SWIFT_TOOLS_PATH"
```

## Notes on the implementation

Here we're describing some decisions that were made while developing
the implementation.

### Relative Pointers

Relative pointers are used in Swift runtime, but currently it's not feasible to use them for some cases
where the pointee is a function pointer. The reason is that WebAssembly has a separate address space for
functions and data, and the offset bwtween a function and a data pointer cannot be defined. Therefore,
we have to use absolute pointers for function pointers in WebAssembly (see `include/swift/ABI/CompactFunctionPointer.h`
for more details).

