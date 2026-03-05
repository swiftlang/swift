// RUN: %swift -typecheck %s -verify -target wasm32-unknown-emscripten -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename %s -target wasm32-unknown-emscripten

#if arch(wasm32) && os(Emscripten) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_32)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64)
class C {}
var x = C()
#endif
#endif
var y = x
