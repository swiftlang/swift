// RUN: %swift -typecheck %s -verify -D FOO -D BAR -target x86_64-apple-macosx10.9 -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-apple-macosx10.9

#if arch(x86_64) && os(OSX) && _runtime(_ObjC) && _endian(little)
class C {}
var x = C()
#endif
var y = x


#if arch(x86_64) && os(macOS) && _runtime(_ObjC) && _endian(little)
class CC {}
var xx = CC()
#endif
var yy = xx
