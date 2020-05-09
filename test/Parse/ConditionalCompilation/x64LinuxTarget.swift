// RUN: %swift -typecheck %s -verify -target x86_64-unknown-linux-gnu -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-unknown-linux-gnu

#if arch(x86_64) && os(Linux) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
