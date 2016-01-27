// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s
// REQUIRES: objc_interop

// Note: no 'import Foundation'

struct DoesNotBridgeToObjC {}

// CHECK-LABEL: sil hidden [noinline] @_TTSf4g___TF31cast_folding_objc_no_foundation23testAnyObjectToArrayIntFPs9AnyObject_Sb
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Array<Int>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Array<Int> in [[TARGET]] : $*Array<Int>, bb1, bb2
@inline(never)
func testAnyObjectToArrayInt(a: AnyObject) -> Bool {
  return a is [Int]
}

// CHECK-LABEL: sil hidden [noinline] @_TTSf4g___TF31cast_folding_objc_no_foundation26testAnyObjectToArrayStringFPs9AnyObject_Sb
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Array<String>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Array<String> in [[TARGET]] : $*Array<String>, bb1, bb2
@inline(never)
func testAnyObjectToArrayString(a: AnyObject) -> Bool {
  return a is [String]
}

// CHECK-LABEL: sil hidden [noinline] @_TTSf4dg___TF31cast_folding_objc_no_foundation30testAnyObjectToArrayNotBridgedFPs9AnyObject_Sb
// CHECK-NEXT: bb0:
// CHECK: [[VALUE:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK: [[RESULT:%.*]] = struct $Bool ([[VALUE]] : $Builtin.Int1)
// CHECK: return [[RESULT]]
@inline(never)
func testAnyObjectToArrayNotBridged(a: AnyObject) -> Bool {
  return a is [DoesNotBridgeToObjC]
}

// CHECK-LABEL: sil hidden [noinline] @_TTSf4g___TF31cast_folding_objc_no_foundation25testAnyObjectToDictionaryFPs9AnyObject_Sb
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Dictionary<Int, String>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Dictionary<Int, String> in [[TARGET]] : $*Dictionary<Int, String>, bb1, bb2
@inline(never)
func testAnyObjectToDictionary(a: AnyObject) -> Bool {
  return a is [Int:String]
}

// CHECK-LABEL: sil hidden [noinline] @_TTSf4g___TF31cast_folding_objc_no_foundation21testAnyObjectToStringFPs9AnyObject_Sb
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $String
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to String in [[TARGET]] : $*String, bb1, bb2
@inline(never)
func testAnyObjectToString(a: AnyObject) -> Bool {
  return a is String
}

class SomeObject {}

print(testAnyObjectToArrayInt(SomeObject()))
print(testAnyObjectToArrayString(SomeObject()))
print(testAnyObjectToArrayNotBridged(SomeObject()))
print(testAnyObjectToDictionary(SomeObject()))
print(testAnyObjectToString(SomeObject()))
