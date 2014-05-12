// RUN: %swift -parse %s -verify

class A {
	var x = 0
}

class B : A, _BridgedToObjectiveC {
	func bridgeToObjectiveC() -> A {
		return A()
	}
}

var a: A[] = []
var b: B[] = []

a = b
b = a // expected-error {{cannot convert the expression's type '()' to type 'B[]'}}

var aa: A[][] = []
var bb: B[][] = []

aa = bb // expected-error {{cannot convert the expression's type '()' to type '(A[])[]'}}

class C {
	
}

// In this case, bridged conversion should win
class E {
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

class G {
	var x = 0
}

class H : G, _ConditionallyBridgedToObjectiveC {
	func bridgeToObjectiveC() -> G {
		return self
	}

	class func isBridgedToObjectiveC() -> Bool {
		return false
	}
}

var g: G[] = []
var h: H[] = []

g = h // should type check, but cause a failure at runtime


class I : _BridgedToObjectiveC {
	func bridgeToObjectiveC() -> AnyObject {
		return self
	}
}

var aoa: AnyObject[] = []
var i: I[] = []

aoa = i
i = aoa // expected-error {{cannot convert the expression's type '()' to type 'I[]'}}
