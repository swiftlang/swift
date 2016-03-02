// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

class A {
  var x = 0
}

struct B : _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  static func _getObjectiveCType() -> Any.Type {
    return A.self
  }
  func _bridgeToObjectiveC() -> A {
    return A()
  }
  static func _forceBridgeFromObjectiveC(
    x: A,
    result: inout B?
  ){
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: A,
    result: inout B?
  ) -> Bool {
    return true
  }
}

var a: [A] = []
var b: [B] = []

a = b

b = a // expected-error {{cannot assign value of type '[A]' to type '[B]'}}

var aa: [[A]] = []
var bb: [[B]] = []

aa = bb // expected-error {{cannot assign value of type '[[B]]' to type '[[A]]'}}

class C {
}

// In this case, bridged conversion should win
class E {
  var x = 0
}

struct F : _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  static func _getObjectiveCType() -> Any.Type {
    return E.self
  }
  func _bridgeToObjectiveC() -> E {
    return E()
  }
  static func _forceBridgeFromObjectiveC(
    x: E,
    result: inout F?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: E,
    result: inout F?
  ) -> Bool {
    return true
  }
}

var e: [E] = []
var f: [F] = []

e = f
f = e // expected-error {{cannot assign value of type '[E]' to type '[F]'}}

class G {
  var x = 0
}

struct H : _ObjectiveCBridgeable {
  static func _getObjectiveCType() -> Any.Type {
    return G.self
  }
  func _bridgeToObjectiveC() -> G {
    return G()
  }
  static func _forceBridgeFromObjectiveC(
    x: G,
    result: inout H?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: G,
    result: inout H?
  ) -> Bool {
    return true
  }
  static func _isBridgedToObjectiveC() -> Bool {
    return false
  }
}

var g: [G] = []
var h: [H] = []

g = h // should type check, but cause a failure at runtime


struct I : _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  static func _getObjectiveCType() -> Any.Type {
    return A.self
  }
  func _bridgeToObjectiveC() -> AnyObject {
    return A()
  }
  static func _forceBridgeFromObjectiveC(
    x: AnyObject,
    result: inout I?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: AnyObject,
    result: inout I?
  ) -> Bool {
    return true
  }
}

var aoa: [AnyObject] = []
var i: [I] = []

aoa = i
i = aoa // expected-error {{cannot assign value of type '[AnyObject]' to type '[I]'}}
