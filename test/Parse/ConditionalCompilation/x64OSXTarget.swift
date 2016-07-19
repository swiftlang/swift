// RUN: %swift -parse %s -verify -D FOO -D BAR -target x86_64-apple-macosx10.9 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-apple-macosx10.9

#if arch(x86_64) && os(OSX) && _runtime(_ObjC) && _endian(little)
class C {}
var x = C()
#endif
var y = x
