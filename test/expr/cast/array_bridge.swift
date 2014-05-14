// RUN: %swift -parse %s -verify

class A {
  var x = 0
}

class B : A, _BridgedToObjectiveC {
  class func getObjectiveCType() -> Any.Type {
    return A.self
  }
  func bridgeToObjectiveC() -> A {
    return A()
  }
  class func bridgeFromObjectiveC(x: A) -> B {
    fatal("implement")
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
  class func getObjectiveCType() -> Any.Type {
    return E.self
  }
  func bridgeToObjectiveC() -> E {
    return self
  }
  class func bridgeFromObjectiveC(x: E) -> F {
    fatal("implement")
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
  class func getObjectiveCType() -> Any.Type {
    return G.self
  }
  func bridgeToObjectiveC() -> G {
    return self
  }
  class func bridgeFromObjectiveC(x: G) -> H {
    fatal("implement")
  }
  class func isBridgedToObjectiveC() -> Bool {
    return false
  }
}

var g: G[] = []
var h: H[] = []

g = h // should type check, but cause a failure at runtime


class I : _BridgedToObjectiveC {
  class func getObjectiveCType() -> Any.Type {
    return AnyObject.self
  }
  func bridgeToObjectiveC() -> AnyObject {
    return self
  }
  class func bridgeFromObjectiveC(x: AnyObject) -> I {
    fatal("implement")
  }
}

var aoa: AnyObject[] = []
var i: I[] = []

aoa = i
i = aoa // expected-error {{cannot convert the expression's type '()' to type 'I[]'}}
