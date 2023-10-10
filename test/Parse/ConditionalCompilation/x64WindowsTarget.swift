// RUN: %swift -typecheck %s -verify -target x86_64-unknown-windows-msvc -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-unknown-windows-msvc

#if arch(x86_64) && os(Windows) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64) && _hasAtomicBitWidth(_128)
class C {}
var x = C()
#endif
#endif
var y = x
