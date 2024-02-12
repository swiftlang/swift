// RUN: %swift -typecheck %s -verify -target x86_64-apple-macosx10.9 -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-apple-macosx10.9

#if arch(x86_64) && os(OSX) && _runtime(_ObjC) && _endian(little) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64) && _hasAtomicBitWidth(_128)
class C {}
var x = C()
#endif
#endif
var y = x


#if arch(x86_64) && os(macOS) && _runtime(_ObjC) && _endian(little) && _pointerBitWidth(_64)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64) && _hasAtomicBitWidth(_128)
class CC {}
var xx = CC()
#endif
#endif
var yy = xx
