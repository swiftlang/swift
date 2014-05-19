// RUN: %swift -parse %s -verify

class A {
  var x = 0
}

struct B : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return A.self
  }
  func bridgeToObjectiveC() -> A {
    return A()
  }
  static func bridgeFromObjectiveC(x: A) -> B? {
    _preconditionFailure("implement")
  }
}

var a: A[] = []
var b: B[] = []

a = b
b = a // expected-error {{cannot convert the expression's type '()' to type 'B[]'}}

var aa: A[][] = []
var bb: B[][] = []

aa = bb // expected-error {{cannot convert the expression's type '()' to type 'A[][]'}}

class C {
}

// In this case, bridged conversion should win
class E {
  var x = 0
}

struct F : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return E.self
  }
  func bridgeToObjectiveC() -> E {
    return E()
  }
  static func bridgeFromObjectiveC(x: E) -> F? {
    _preconditionFailure("implement")
  }
}

var e: E[] = []
var f: F[] = []

e = f
f = e // expected-error {{cannot convert the expression's type '()' to type 'F[]'}}

class G {
  var x = 0
}

struct H : _ConditionallyBridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return G.self
  }
  func bridgeToObjectiveC() -> G {
    return G()
  }
  static func bridgeFromObjectiveC(x: G) -> H? {
    _preconditionFailure("implement")
  }
  static func isBridgedToObjectiveC() -> Bool {
    return false
  }
}

var g: G[] = []
var h: H[] = []

g = h // should type check, but cause a failure at runtime


struct I : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return A.self
  }
  func bridgeToObjectiveC() -> AnyObject {
    return A()
  }
  static func bridgeFromObjectiveC(x: AnyObject) -> I? {
    _preconditionFailure("implement")
  }
}

var aoa: AnyObject[] = []
var i: I[] = []

aoa = i
i = aoa // expected-error {{cannot convert the expression's type '()' to type 'I[]'}}
