SwiftWasm: Getting started
==========================

Thank you for trying SwiftWasm! Here's how to get started.

Please visit our website at https://swiftwasm.org for the latest updates.


Install dependencies
====================

Before running SwiftWasm, you will need to install some dependencies.

Ubuntu:

```
sudo apt-get install libatomic1
```

macOS:

(No dependencies needed.)

Windows:

Install Windows Subsystem for Linux, then follow the Ubuntu instructions.




Compile SwiftWasm
=================

Run

```
./swiftwasm example/hello.swift hello.wasm
```

To compile example/hello.swift to hello.wasm.




Running Wasm files
==================

To run the resulting hello.wasm file:

- Visit https://swiftwasm.org/polyfill/
- select "Browse", and choose the hello.wasm file
- you should get output in the textbox.

This polyfill should work in Firefox 66, Chrome 74, and Safari 12.1.

You can also run the file outside a browser with:

- Wasmtime https://github.com/CraneStation/wasmtime
- Lucet https://github.com/swiftwasm/lucet/tree/swiftwasm
- or any other WASI-compatible WebAssembly runtime.




Questions and support
=====================

If you have any questions, please open an issue on

https://github.com/swiftwasm/swift



Third-party licenses
====================

This package contains components with their own license requirements.

Swift compiler: https://github.com/apple/swift/blob/master/LICENSE.txt

LLVM/Clang: https://github.com/llvm/llvm-project/blob/master/lld/LICENSE.TXT

WASI sysroot: https://github.com/CraneStation/wasi-sysroot/blob/master/LICENSE

ICU: https://github.com/unicode-org/icu/blob/master/icu4c/LICENSE

WASI polyfill: https://github.com/CraneStation/wasmtime/blob/master/wasmtime-wasi/LICENSE
