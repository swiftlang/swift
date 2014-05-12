// RUN: %swift -parse %s -verify -D FOO -D BAR -target arm64-apple-ios7.0 -D FOO

#if arch(arm64) && os(iOS)
class C {}
var x = C()
#endif
var y = x
