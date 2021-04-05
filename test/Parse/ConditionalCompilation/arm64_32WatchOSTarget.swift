// RUN: %swift -typecheck %s -verify -target arm64_32-apple-watchos7.0 -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm64_32-apple-watchos7.0

#if os(iOS)
// This block should not parse.
// os(tvOS) or os(watchOS) does not imply os(iOS).
let i: Int = "Hello"
#endif

#if arch(arm64_32) && os(watchOS) && _runtime(_ObjC) && _endian(little)
class C {}
var x = C()
#endif
var y = x
