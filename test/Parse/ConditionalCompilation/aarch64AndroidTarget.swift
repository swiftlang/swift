// RUN: %swift -typecheck %s -verify -target aarch64-unknown-linux-android -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target aarch64-unknown-linux-android

#if os(Linux) || os(anyAppleOS)
// This block should not be type checked.
// os(Android) does not imply os(Linux).
let i: Int = "Hello"
#endif

#if arch(arm64) && os(Android) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64) && _hasAtomicBitWidth(_128)
class C {}
var x = C()
#endif
#endif
var y = x
