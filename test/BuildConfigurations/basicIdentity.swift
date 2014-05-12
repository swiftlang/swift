// RUN: %swift -parse %s -verify -D FOO -D BAZ -target x86_64-apple-darwin11.3.0

#if FOO
var a = 0
#endif
var b = a

#if !BAR
var c = 0
#endif
var d = c

#if FOO || BAR
var e = 0
#endif
var f = e

#if BAR || FOO
var g = 0
#endif
var h = g

#if FOO && BAZ
var i = 0
#endif
var j = i

#if os(OSX)
var k = 0
#endif
var l = k

#if arch(x86_64)
var m = 0
#endif
var n = m

#if FOO && !BAR && BAZ && os(OSX) && arch(x86_64)
var o = 0
#endif
var p = o

#if FOO && (!BAR && BAZ && os(OSX) && arch(x86_64))
var q = 0
#endif
var r = q

#if FOO && !(!BAZ && BAZ && os(OSX) && arch(x86_64))
var s = 0
#endif
var t = s
