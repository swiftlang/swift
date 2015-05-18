// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s
// We want to check two things here:
// - Correctness
// - That certain "is" checks are eliminated based on static analysis at compile-time
//
// In ideal world, all those testNN functions should be simplified down to a single basic block
// which returns either true or false, i.e. all type checks should folded statically.

// REQUIRES: objc_interop

import Foundation

class ObjCX : NSObject {}

struct CX: _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static func _getObjectiveCType() -> Any.Type {
    return String.self
  }

  func _bridgeToObjectiveC() -> ObjCX {
    return ObjCX()
  }

  static func _forceBridgeFromObjectiveC(source: ObjCX, inout result: CX?) {}

  static func _conditionallyBridgeFromObjectiveC(source: ObjCX, inout result: CX?) -> Bool {
    return false
  }
}

// Check casts to types which are _ObjectiveCBridgeable
func cast0(o: AnyObject) -> Bool {
  return o is CX
}

// CHECK-LABEL: sil hidden [noinline] @_TF17cast_folding_objc5test0FT_Sb
// CHECK: bb0
// Check that cast is not elmiminated even though cast0 is a conversion
// from a class to struct, because it casts to a struct implementing
// the _BridgedToObjectiveC protocol
// CHECK: checked_cast
// CHECK: return
@inline(never)
func test0() -> Bool {
  return cast0(NSNumber(integer:1))
}

// Check that compiler understands that this cast always succeeds.
// Since it is can be statically proven that NSString is bridgeable to String,
// _forceBridgeFromObjectiveC from String should be invoked instead of
// a more general, but less effective swift_bridgeNonVerbatimFromObjectiveC, which
// also performs conformance checks at runtime.
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc30testBridgedCastFromObjCtoSwiftFCSo8NSStringSS
// CHECK-NOT: cast
// CHECK: witness_method $String, #_ObjectiveCBridgeable._forceBridgeFromObjectiveC!1
// CHECK: metatype $@thick String.Type
// CHECK: apply
// CHECK: return
@inline(never)
public func testBridgedCastFromObjCtoSwift(ns: NSString) -> String {
  return ns as String
}

// Check that compiler understands that this cast always succeeds
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc30testBridgedCastFromSwiftToObjCFSSCSo8NSString
// CHECK-NOT: cast
// CHECK: function_ref @_TFE10FoundationSS19_bridgeToObjectiveCfSSFT_CSo8NSString
// CHECK: apply
// CHECK: return
@inline(never)
public func testBridgedCastFromSwiftToObjC(s: String) -> NSString {
  return s as NSString
}

// Check that this cast does not get eliminated, because
// the compiler does not statically know if this object
// is NSNumber can can be converted into Int.
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc35testMayBeBridgedCastFromObjCtoSwiftFPSs9AnyObject_Si
// CHECK: unconditional_checked_cast_addr
// CHECK: return
@inline(never)
public func testMayBeBridgedCastFromObjCtoSwift(o: AnyObject) -> Int {
  return o as! Int
}

// Check that this cast does not get eliminated, because
// the compiler does not statically know if this object
// is NSNumber can can be converted into Int.
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc41testConditionalBridgedCastFromObjCtoSwiftFPSs9AnyObject_GSqSS_
// CHECK: unconditional_checked_cast_addr
// CHECK: return
@inline(never)
public func testConditionalBridgedCastFromObjCtoSwift(o: AnyObject) -> String? {
  return o as? String
}

public func castObjCToSwift<T>(t: T) -> Int {
  return t as! Int
}

// Check that compiler understands that this cast always fails
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc37testFailingBridgedCastFromObjCtoSwiftFCSo8NSStringSi
// CHECK: builtin "int_trap"
// CHECK-NEXT: unreachable
// CHECK-NEXT: }
@inline(never)
public func testFailingBridgedCastFromObjCtoSwift(ns: NSString) -> Int {
  return castObjCToSwift(ns)
}

// Check that compiler understands that this cast always fails
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc37testFailingBridgedCastFromSwiftToObjCFSSSi
// CHECK: builtin "int_trap"
// CHECK-NEXT: unreachable
// CHECK-NEXT: }
@inline(never)
public func testFailingBridgedCastFromSwiftToObjC(s: String) -> NSInteger {
  return s as! NSInteger
}

print("test0=\(test0())")
