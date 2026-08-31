// RUN: %swift -typecheck %s -verify -target wasm32-unknown-wasi -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename %s -target wasm32-unknown-wasi

// expected-warning@<unknown> * {{libc not found for 'wasm32-unknown-wasi'; C stdlib may be unavailable}}
// expected-warning@<unknown> * {{argument '--target=wasm32-wasi' is deprecated, use --target=wasm32-wasip1 instead}}

#if arch(wasm32) && os(WASI) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_32)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64)
class C {}
var x = C()
#endif
#endif
var y = x

#if !_runtime(_multithreaded)
  let z = xx // expected-error {{cannot find 'xx' in scope}}
#endif
