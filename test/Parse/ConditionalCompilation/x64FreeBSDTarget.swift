// RUN: %swift -typecheck %s -verify -target x86_64-unknown-freebsd10 -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-unknown-freebsd10

#if arch(x86_64) && os(FreeBSD) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
