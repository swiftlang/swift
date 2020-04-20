Creates packages containing everything needed to build WebAssembly programs with Swift.

# Building

```
./download-prebuilts.sh
./download-installable-prebuilts.sh
./build-packages.sh
```

# Contents of package

- Swift toolchain from [swiftwasm-sdk](https://github.com/swiftwasm/swiftwasm-sdk)
- WASI modified sysroot from [wasi-sdk](https://github.com/swiftwasm/wasi-sdk)
- libicu from [icu4c-wasi](https://github.com/swiftwasm/icu4c-wasi)
- linking helpers from [swiftwasm-wasi-stubs](https://github.com/swiftwasm/swiftwasm-wasi-stubs)
- wasi-ld, either from wasi-sdk (on Linux) or upstream LLVM 9.0 (on Mac)
- build script for compiling a Swift file to a .wasm
- a Getting Started guide

# Notes

This shares a lot with the [swiftwasm-compile-service](https://github.com/swiftwasm/swiftwasm-compile-service).
