// RUN: %swift -parse %s -verify

class A : ObjCClassType {
	var x = 0
}

class B : _BridgedToObjectiveC {
	func bridgeToObjectiveC() -> A {
		return A()
	}
}

var a: A[] = []
var b: B[] = []

a = b
b = a // expected-error {{cannot convert the expression's type '()' to type 'B[]'}}

// In this case, bridged conversion should win
class E : ObjCClassType {
	var x = 0
}

class F : E, _BridgedToObjectiveC {
	func bridgeToObjectiveC() -> E {
		return self
	}
}

var e: E[] = []
var f: F[] = []

e = f
f = e // expected-error {{cannot convert the expression's type '()' to type 'F[]'}}
