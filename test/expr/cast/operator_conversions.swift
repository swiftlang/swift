// RUN: %swift -parse %s -verify

var a = [1]
var b = [1]

a == a // expected-error{{could not find an overload for '==' that accepts the supplied arguments}}
a == b // expected-error{{could not find an overload for '==' that accepts the supplied arguments}}
b == a // expected-error{{could not find an overload for '==' that accepts the supplied arguments}}

class C {}
class D { 
	@conversion func __conversion() -> C { return C() }
}

var c = C()
var d = D()

func == (l: C, r: C) -> Bool { return true }

d == d // expected-error{{could not find an overload for '==' that accepts the supplied arguments}}
c == c // no error
c == d // no error, because of conversion on the rhs
d == c // no error, because of conversion on the lhs