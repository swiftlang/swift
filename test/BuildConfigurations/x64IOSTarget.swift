// RUN: %swift -parse %s -verify -D FOO -D BAR -target x86_64-apple-ios7.0 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-apple-ios7.0

#if arch(x86_64) && os(iOS)
class C {}
var x = C()
#endif
var y = x
