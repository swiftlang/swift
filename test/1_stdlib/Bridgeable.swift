// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: interpret
// REQUIRES: objc_interop

// CHECK: testing...
println("testing...")

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

func testBridging<T>(x: T, _ name: String) {
  println("\(name) \(bridgedStatus(T.self))")
  var b : String
  if let result? = _bridgeToObjectiveC(x) {
    b = "bridged as " + (
      (result as? C) != nil ? "C" : (result as? T) != nil ? "itself" : "an unknown type")
  }
  else {
    b = "did not bridge"
  }
  println("\(name) instance \(b)")
}

//===----------------------------------------------------------------------===//
struct BridgedValueType : _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  static func _getObjectiveCType() -> Any.Type {
    return C.self
  }
  func _bridgeToObjectiveC() -> C {
    return C()
  }
  static func _forceBridgeFromObjectiveC(
    x: C,
    inout result: BridgedValueType?
  ) {
    _preconditionFailure("implement")
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: C,
    inout result: BridgedValueType?
  ) -> Bool {
    _preconditionFailure("implement")
  }
}

// CHECK-NEXT: BridgedValueType is custom-bridged
// CHECK-NEXT: BridgedValueType instance bridged as C
testBridging(BridgedValueType(), "BridgedValueType")

//===----------------------------------------------------------------------===//
struct UnbridgedValueType {}

// CHECK-NEXT: UnbridgedValueType is unbridged
// CHECK-NEXT: UnbridgedValueType instance did not bridge
testBridging(UnbridgedValueType(), "UnbridgedValueType")
  
//===----------------------------------------------------------------------===//
class PlainClass {}

// CHECK-NEXT: PlainClass is bridged verbatim
// CHECK-NEXT: PlainClass instance bridged as itself
testBridging(PlainClass(), "PlainClass")

//===----------------------------------------------------------------------===//
struct ConditionallyBridged<T> : _ObjectiveCBridgeable {
  static func _getObjectiveCType() -> Any.Type {
    return C.self
  }
  func _bridgeToObjectiveC() -> C {
    return C()
  }
  static func _forceBridgeFromObjectiveC(
    x: C,
    inout result: ConditionallyBridged<T>?
  ) {
    _preconditionFailure("implement")
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: C,
    inout result: ConditionallyBridged<T>?
  ) -> Bool {
    _preconditionFailure("implement")
  }
  static func _isBridgedToObjectiveC() -> Bool {
    return ((T.self as Any) as? String.Type) == nil
  }
}

// CHECK-NEXT: ConditionallyBridged<Int> is custom-bridged
// CHECK-NEXT: ConditionallyBridged<Int> instance bridged as C
testBridging(ConditionallyBridged<Int>(), "ConditionallyBridged<Int>")

// CHECK-NEXT: ConditionallyBridged<String> is unbridged
// CHECK-NEXT: ConditionallyBridged<String> instance did not bridge
testBridging(
  ConditionallyBridged<String>(), "ConditionallyBridged<String>")

// CHECK-NEXT: done.
println("done.")
