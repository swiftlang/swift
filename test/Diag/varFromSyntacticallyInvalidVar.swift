// RUN: %swift %s -parse -verify
// XFAIL: *
var a: = 1 // expected-error {{expected type}}
var b = a
println(a)
println(b)
