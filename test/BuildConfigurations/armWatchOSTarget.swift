// RUN: %swift -parse %s -verify -D FOO -D BAR -target arm-apple-watchos2.0 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm-apple-watchos2.0

#if arch(arm) && os(watchOS) && _runtime(_ObjC)
class C {}
var x = C()
#endif
var y = x
