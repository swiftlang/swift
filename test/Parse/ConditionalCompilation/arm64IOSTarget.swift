// RUN: %swift -typecheck %s -verify -D FOO -D BAR -target arm64-apple-ios7.0 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm64-apple-ios7.0

#if os(tvOS) || os(watchOS)
// This block should not parse.
// os(iOS) does not imply os(TVOS) or os(watchOS).
let i: Int = "Hello"
#endif

#if arch(arm64) && os(iOS) && _runtime(_ObjC) && _endian(little)
class C {}
var x = C()
#endif
var y = x
