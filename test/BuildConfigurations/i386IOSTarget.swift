// RUN: %swift -parse %s -verify -D FOO -D BAR -target i386-apple-ios7.0 -D FOO

#if arch(i386) && os(iOS)
class C {}
var x = C()
#endif
var y = x
