// RUN: %swift -typecheck %s -verify -target s390x-unknown-linux-gnu -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target s390x-unknown-linux-gnu

// expected-warning@<unknown> * {{libc not found for 's390x-unknown-linux-gnu'; C stdlib may be unavailable}}

#if arch(s390x) && os(Linux) && _runtime(_Native) && _endian(big) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64)
class C {}
var x = C()
#endif
#endif
var y = x
