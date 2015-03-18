// RUN: %swift -parse %s -verify -D FOO -D BAR -target arm64-apple-tvos9.0 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm64-apple-tvos9.0

#if arch(arm64) && os(tvOS) && _runtime(_ObjC)
class C {}
var x = C()
#endif
var y = x
