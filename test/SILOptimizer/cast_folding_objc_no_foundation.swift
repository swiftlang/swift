// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -O -emit-sil %s | %FileCheck %s
// REQUIRES: objc_interop

// TODO: Update optimizer for id-as-Any changes.

// Note: no 'import Foundation'

struct PlainStruct {}

// CHECK-LABEL: sil hidden [noinline] @_T031cast_folding_objc_no_foundation23testAnyObjectToArrayIntSbs0gH0_pFTf4g_n
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Array<Int>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Array<Int> in [[TARGET]] : $*Array<Int>, bb1, bb2
@inline(never)
func testAnyObjectToArrayInt(_ a: AnyObject) -> Bool {
  return a is [Int]
}

// CHECK-LABEL: sil hidden [noinline] @_T031cast_folding_objc_no_foundation26testAnyObjectToArrayStringSbs0gH0_pFTf4g_n
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Array<String>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Array<String> in [[TARGET]] : $*Array<String>, bb1, bb2
@inline(never)
func testAnyObjectToArrayString(_ a: AnyObject) -> Bool {
  return a is [String]
}

// CHECK-LABEL: sil hidden [noinline] @_T031cast_folding_objc_no_foundation30testAnyObjectToArrayNotBridged{{.*}}
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Array<PlainStruct>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Array<PlainStruct> in [[TARGET]] : $*Array<PlainStruct>, bb1, bb2
@inline(never)
func testAnyObjectToArrayNotBridged(_ a: AnyObject) -> Bool {
  return a is [PlainStruct]
}

// CHECK-LABEL: sil hidden [noinline] @_T031cast_folding_objc_no_foundation25testAnyObjectToDictionarySbs0gH0_pFTf4g_n
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $Dictionary<Int, String>
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to Dictionary<Int, String> in [[TARGET]] : $*Dictionary<Int, String>, bb1, bb2
@inline(never)
func testAnyObjectToDictionary(_ a: AnyObject) -> Bool {
  return a is [Int: String]
}

// CHECK-LABEL: sil hidden [noinline] @_T031cast_folding_objc_no_foundation21testAnyObjectToStringSbs0gH0_pFTf4g_n
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: [[TARGET:%.*]] = alloc_stack $String
// CHECK: checked_cast_addr_br take_always AnyObject in [[SOURCE]] : $*AnyObject to String in [[TARGET]] : $*String, bb1, bb2
@inline(never)
func testAnyObjectToString(_ a: AnyObject) -> Bool {
  return a is String
}

class SomeObject {}

print(testAnyObjectToArrayInt(SomeObject()))
print(testAnyObjectToArrayString(SomeObject()))
print(testAnyObjectToArrayNotBridged(SomeObject()))
print(testAnyObjectToDictionary(SomeObject()))
print(testAnyObjectToString(SomeObject()))
