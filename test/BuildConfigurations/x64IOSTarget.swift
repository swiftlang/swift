// RUN: %swift -parse %s -verify -D FOO -D BAR -target x86_64-apple-ios7.0 -D FOO

#if arch(x86_64) && os(iOS)
class C {}
var x = C()
#endif
var y = x
