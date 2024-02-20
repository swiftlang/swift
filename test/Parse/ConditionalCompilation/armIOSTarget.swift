// RUN: %swift -typecheck %s -verify -target arm-apple-ios7.0 -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm-apple-ios7.0

#if os(tvOS) || os(watchOS)
// This block should not parse.
// os(iOS) does not imply os(tvOS) or os(watchOS).
let i: Int = "Hello"
#endif

#if arch(arm) && os(iOS) && _runtime(_ObjC) && _endian(little) && _pointerBitWidth(_32)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32) && _hasAtomicBitWidth(_64)
class C {}
var x = C()
#endif
#endif
var y = x
