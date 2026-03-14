// RUN: %swift -typecheck %s -verify -target x86_64-scei-ps4 -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename %s -target x86_64-scei-ps4

#if os(FreeBSD)
// This block should not parse.
// os(FreeBSD) does not imply os(PS4)
let i: Int = "Hello"
#endif

#if arch(x86_64) && os(PS4) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64) && _hasAtomicBitWidth(_128)
class C {}
var x = C()
#endif
#endif

var y = x

