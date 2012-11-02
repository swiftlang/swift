// RUN: %swift %s -parse -verify
// XFAIL: *
var a // expected-error {{type annotation missing in pattern}}
var b = a
println(a)
println(b)
