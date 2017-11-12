// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

// FIXME: Should go into the standard library.
public extension _ObjectiveCBridgeable {
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    var result: Self?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}

class A {
  var x = 0
}

struct B : _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> A {
    return A()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: A,
    result: inout B?
  ){
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: A,
    result: inout B?
  ) -> Bool {
    return true
  }
}

var a: [A] = []
var b: [B] = []

a = b as [A]

b = a // expected-error {{cannot assign value of type '[A]' to type '[B]'}}

var aa: [[A]] = []
var bb: [[B]] = []

aa = bb // expected-error {{cannot assign value of type '[[B]]' to type '[[A]]'}}
bb = aa // expected-error {{cannot assign value of type '[[A]]' to type '[[B]]'}}

class C {
}

// In this case, bridged conversion should win
class E {
  var x = 0
}

struct F : _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> E {
    return E()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: E,
    result: inout F?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: E,
    result: inout F?
  ) -> Bool {
    return true
  }
}

var e: [E] = []
var f: [F] = []

e = f as [E]
f = e // expected-error {{cannot assign value of type '[E]' to type '[F]'}}

class G {
  var x = 0
}

struct H : _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> G {
    return G()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: G,
    result: inout H?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: G,
    result: inout H?
  ) -> Bool {
    return true
  }
}

var g: [G] = []
var h: [H] = []

g = h as [G] // should type check, but cause a failure at runtime


struct I : _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> AnyObject {
    return A()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: AnyObject,
    result: inout I?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: AnyObject,
    result: inout I?
  ) -> Bool {
    return true
  }
}

var aoa: [AnyObject] = []
var i: [I] = []

aoa = i as [AnyObject]
i = aoa // expected-error {{cannot assign value of type '[AnyObject]' to type '[I]'}}
