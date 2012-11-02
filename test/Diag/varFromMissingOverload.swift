// RUN: %swift %s -parse -verify
// XFAIL: *
func foo(x:Int) -> Int { return x }
var a = foo("not an int") // expected-error {{inferred type 'Int' is not compatible with literal}}
var b = a + a
