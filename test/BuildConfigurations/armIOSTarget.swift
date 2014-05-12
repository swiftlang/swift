// RUN: %swift -parse %s -verify -D FOO -D BAR -target arm-apple-ios7.0 -D FOO

#if arch(arm) && os(iOS)
class C {}
var x = C()
#endif
var y = x
