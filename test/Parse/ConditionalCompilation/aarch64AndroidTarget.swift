// RUN: %swift -typecheck %s -verify -target aarch64-none-linux-android -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target aarch64-none-linux-android

#if os(Linux)
// This block should not parse.
// os(Android) does not imply os(Linux).
let i: Int = "Hello"
#endif

#if arch(arm64) && os(Android) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
