// RUN: %swift -typecheck %s -verify -D FOO -D BAR -target x86_64-unknown-linux-gnu -disable-objc-interop -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-unknown-linux-gnu

#if arch(x86_64) && os(Linux) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
