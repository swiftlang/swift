// RUN: %swift -typecheck %s -verify -target x86_64-unknown-windows-msvc -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-unknown-windows-msvc

#if arch(x86_64) && os(Windows) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
