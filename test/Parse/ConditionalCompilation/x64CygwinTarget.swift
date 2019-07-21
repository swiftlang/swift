// RUN: %swift -typecheck %s -verify -target x86_64-unknown-windows-cygnus -disable-objc-interop -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-unknown-windows-cygnus

#if arch(x86_64) && os(Cygwin) && _runtime(_Native) && _endian(little)
class C {}
var x = C()
#endif
var y = x
