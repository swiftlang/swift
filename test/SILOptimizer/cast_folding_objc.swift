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
// Check that cast is not eliminated even though cast0 is a conversion
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
// CHECK: function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
// CHECK: apply
// CHECK: return
@inline(never)
public func testBridgedCastFromSwiftToObjC(s: String) -> NSString {
  return s as NSString
}

// Check that this cast does not get eliminated, because
// the compiler does not statically know if this object
// is NSNumber can can be converted into Int.
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc35testMayBeBridgedCastFromObjCtoSwiftFPs9AnyObject_Si
// CHECK: unconditional_checked_cast_addr
// CHECK: return
@inline(never)
public func testMayBeBridgedCastFromObjCtoSwift(o: AnyObject) -> Int {
  return o as! Int
}

// Check that this cast does not get eliminated, because
// the compiler does not statically know if this object
// is NSString can can be converted into String.
// CHECK-LABEL: sil [noinline] @_TF17cast_folding_objc41testConditionalBridgedCastFromObjCtoSwiftFPs9AnyObject_GSqSS_
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

// Check that class instances may be cast to potentially-class metatypes.
// CHECK-LABEL: sil [noinline] @{{.*}}testCastNSObjectToAnyClass{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastNSObjectToAnyClass(o: NSObject) -> AnyClass {
  return o as! AnyClass
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastNSObjectToClassObject{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastNSObjectToClassObject(o: NSObject) -> NSObject.Type {
  return o as! NSObject.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastNSObjectToAnyType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastNSObjectToAnyType(o: NSObject) -> Any.Type {
  return o as! Any.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastNSObjectToEveryType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastNSObjectToEveryType<T>(o: NSObject) -> T.Type {
  return o as! T.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastNSObjectToNonClassType
// CHECK:         builtin "int_trap"
@inline(never)
public func testCastNSObjectToNonClassType(o: NSObject) -> Int.Type {
  return o as! Int.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyObjectToAnyClass{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyObjectToAnyClass(o: AnyObject) -> AnyClass {
  return o as! AnyClass
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyObjectToClassObject{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyObjectToClassObject(o: AnyObject) -> AnyObject.Type {
  return o as! AnyObject.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyObjectToAnyType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyObjectToAnyType(o: AnyObject) -> Any.Type {
  return o as! Any.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyObjectToEveryType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyObjectToEveryType<T>(o: AnyObject) -> T.Type {
  return o as! T.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyObjectToNonClassType
// CHECK:         builtin "int_trap"
@inline(never)
public func testCastAnyObjectToNonClassType(o: AnyObject) -> Int.Type {
  return o as! Int.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyToAnyClass{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyToAnyClass(o: Any) -> AnyClass {
  return o as! AnyClass
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyToClassObject{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyToClassObject(o: Any) -> AnyObject.Type {
  return o as! AnyObject.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyToAnyType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyToAnyType(o: Any) -> Any.Type {
  return o as! Any.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyToEveryType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyToEveryType<T>(o: Any) -> T.Type {
  return o as! T.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastAnyToNonClassType
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastAnyToNonClassType(o: Any) -> Int.Type {
  return o as! Int.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastEveryToAnyClass{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastEveryToAnyClass<T>(o: T) -> AnyClass {
  return o as! AnyClass
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastEveryToClassObject{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastEveryToClassObject<T>(o: T) -> AnyObject.Type {
  return o as! AnyObject.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastEveryToAnyType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastEveryToAnyType<T>(o: T) -> Any.Type {
  return o as! Any.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastEveryToEveryType{{.*}}
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastEveryToEveryType<T, U>(o: U) -> T.Type {
  return o as! T.Type
}

// CHECK-LABEL: sil [noinline] @{{.*}}testCastEveryToNonClassType
// CHECK:         unconditional_checked_cast_addr
@inline(never)
public func testCastEveryToNonClassType<T>(o: T) -> Int.Type {
  return o as! Int.Type
}

print("test0=\(test0())")
