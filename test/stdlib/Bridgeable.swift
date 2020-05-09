// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

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

// CHECK: testing...
print("testing...")

class C {}

func bridgedStatus<T>(_: T.Type) -> String {
  let bridged = _isBridgedToObjectiveC(T.self)
  let verbatim = _isBridgedVerbatimToObjectiveC(T.self)
  if !bridged && verbatim {
    return "IS NOT BRIDGED BUT IS VERBATIM?!"
  }
  return bridged ? 
    verbatim ? "is bridged verbatim" : "is custom-bridged"
    : "is unbridged"
}

func testBridging<T>(_ x: T, _ name: String) {
  print("\(name) \(bridgedStatus(T.self))")
  var b : String
  let result = _bridgeAnythingToObjectiveC(x)
  b = "bridged as " + (
    result is C ? "C" : result is T ? "itself" : "an unknown type")
  print("\(name) instance \(b)")
}

//===----------------------------------------------------------------------===//
struct BridgedValueType : _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> C {
    return C()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: C,
    result: inout BridgedValueType?
  ) {
    preconditionFailure("implement")
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: C,
    result: inout BridgedValueType?
  ) -> Bool {
    preconditionFailure("implement")
  }
}

// CHECK-NEXT: BridgedValueType is custom-bridged
// CHECK-NEXT: BridgedValueType instance bridged as C
testBridging(BridgedValueType(), "BridgedValueType")

//===----------------------------------------------------------------------===//
struct UnbridgedValueType {}

// CHECK-NEXT: UnbridgedValueType is unbridged
// CHECK-NEXT: UnbridgedValueType instance bridged as itself
testBridging(UnbridgedValueType(), "UnbridgedValueType")
  
//===----------------------------------------------------------------------===//
class PlainClass {}

// CHECK-NEXT: PlainClass is bridged verbatim
// CHECK-NEXT: PlainClass instance bridged as itself
testBridging(PlainClass(), "PlainClass")

