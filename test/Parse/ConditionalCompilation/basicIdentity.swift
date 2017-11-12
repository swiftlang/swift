// RUN: %swift -typecheck %s -verify -D FOO -D BAZ -target x86_64-apple-macosx10.9 -parse-stdlib

struct Foo {}

#if FOO
var a = Foo()
#endif
var b = a

#if !BAR
var c = Foo()
#endif
var d = c

#if FOO || BAR
var e = Foo()
#endif
var f = e

#if BAR || FOO
var g = Foo()
#endif
var h = g

#if FOO && BAZ
var i = Foo()
#endif
var j = i

#if os(OSX)
var k = Foo()
#endif
var l = k

#if arch(x86_64)
var m = Foo()
#endif
var n = m

#if FOO && !BAR && BAZ && os(OSX) && arch(x86_64) && _runtime(_ObjC)
var o = Foo()
#endif
var p = o

#if FOO && (!BAR && BAZ && os(OSX) && arch(x86_64)) && _runtime(_ObjC)
var q = Foo()
#endif
var r = q

#if FOO && !(!BAZ && BAZ && os(OSX) && arch(x86_64)) && _runtime(_ObjC)
var s = Foo()
#endif
var t = s

// Test symmetric version of FOO || BAR from above
#if BAR || FOO
var u = Foo()
#endif
var v = u

// Test symmetric version of FOO && BAR from above
#if BAZ && FOO
var w = Foo()
#endif
var x = w
