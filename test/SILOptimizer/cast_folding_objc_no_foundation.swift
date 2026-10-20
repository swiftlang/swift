
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name cast_folding_objc_no_foundation -O -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s
// REQUIRES: objc_interop

// TODO: Update optimizer for id-as-Any changes.

// Note: no 'import Foundation'

struct PlainStruct {}

// CHECK-LABEL: sil hidden [noinline] @$s31cast_folding_objc_no_foundation23testAnyObjectToArrayIntySbyXlF : $@convention(thin) (@guaranteed AnyObject) -> Bool {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: checked_cast_addr_br test_only AnyObject in [[SOURCE]] : $*AnyObject to Array<Int>, bb2, bb1
@inline(never)
func testAnyObjectToArrayInt(_ a: AnyObject) -> Bool {
  return a is [Int]
}

// CHECK-LABEL: sil hidden [noinline] @$s31cast_folding_objc_no_foundation26testAnyObjectToArrayStringySbyXlF : $@convention(thin) (@guaranteed AnyObject) -> Bool {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: checked_cast_addr_br test_only AnyObject in [[SOURCE]] : $*AnyObject to Array<String>, bb2, bb1
@inline(never)
func testAnyObjectToArrayString(_ a: AnyObject) -> Bool {
  return a is [String]
}

// CHECK-LABEL: sil hidden [noinline] @$s31cast_folding_objc_no_foundation30testAnyObjectToArrayNotBridgedySbyXlF : $@convention(thin) (@guaranteed AnyObject) -> Bool {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: checked_cast_addr_br test_only AnyObject in [[SOURCE]] : $*AnyObject to Array<PlainStruct>, bb2, bb1
@inline(never)
func testAnyObjectToArrayNotBridged(_ a: AnyObject) -> Bool {
  return a is [PlainStruct]
}

// CHECK-LABEL: sil hidden [noinline] @$s31cast_folding_objc_no_foundation25testAnyObjectToDictionaryySbyXlF : $@convention(thin) (@guaranteed AnyObject) -> Bool {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: checked_cast_addr_br test_only AnyObject in [[SOURCE]] : $*AnyObject to Dictionary<Int, String>, bb2, bb1
@inline(never)
func testAnyObjectToDictionary(_ a: AnyObject) -> Bool {
  return a is [Int: String]
}

// CHECK-LABEL: sil hidden [noinline] @$s31cast_folding_objc_no_foundation21testAnyObjectToStringySbyXlF : $@convention(thin) (@guaranteed AnyObject) -> Bool {
// CHECK: bb0(%0 : $AnyObject):
// CHECK: [[SOURCE:%.*]] = alloc_stack $AnyObject
// CHECK: checked_cast_addr_br test_only AnyObject in [[SOURCE]] : $*AnyObject to String, bb2, bb1
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
