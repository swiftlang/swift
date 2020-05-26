// RUN: %swift -typecheck %s -verify -target wasm32-unknown-wasi -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename %s -target wasm32-unknown-wasi

#if arch(wasm32) && os(WASI) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
