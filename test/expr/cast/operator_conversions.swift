// RUN: %swift -parse %s -verify

class C {}
class D { 
	@conversion func __conversion() -> C { return C() }
}

var c = C()
var d = D()

func == (l: C, r: C) -> Bool { return true }

d == d // expected-error{{'D' is not convertible to 'C'}}
c == c // no error
c == d // no error, because of conversion on the rhs
d == c // no error, because of conversion on the lhs