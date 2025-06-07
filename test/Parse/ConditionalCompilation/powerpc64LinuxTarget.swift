// RUN: %swift -typecheck %s -verify -target powerpc64-unknown-linux-gnu -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target powerpc64-unknown-linux-gnu

#if arch(powerpc64) && os(Linux) && _runtime(_Native) && _endian(big) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64)
class C {}
var x = C()
#endif
#endif
var y = x
