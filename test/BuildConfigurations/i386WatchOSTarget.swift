// RUN: %swift -parse %s -verify -D FOO -D BAR -target i386-apple-watchos2.0 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target i386-apple-watchos2.0

#if arch(i386) && os(watchOS) && _runtime(_ObjC)
class C {}
var x = C()
#endif
var y = x
