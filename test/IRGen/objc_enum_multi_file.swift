// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -module-name main -primary-file %s %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir | FileCheck %s

// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module %S/Inputs/objc_enum_multi_file_helper.swift -o %t
// RUN: %target-swift-frontend -module-name main -primary-file %s -I %t -DIMPORT -emit-ir | FileCheck %s

#if IMPORT
import objc_enum_multi_file_helper
#endif

// CHECK-LABEL: define hidden i32 @_TF4main6useFooFO{{S_|27objc_enum_multi_file_helper}}3FooVSs5Int32(i32) {
func useFoo(x: Foo) -> Int32 {
  // CHECK: switch i32 %0, label %[[DEFAULT:.+]] [
  // CHECK-DAG: i32 1, label %[[CASE_B:.+]]
  // CHECK-DAG: i32 2, label %[[CASE_C:.+]]
  // CHECK-DAG: i32 0, label %[[CASE_A:.+]]
  // CHECK: ]

  // CHECK: <label>:[[DEFAULT]]
  // CHECK-NEXT: unreachable

  switch x {
  // CHECK: <label>:[[CASE_B]]
  // CHECK-NEXT: br label %[[RESULT_B:.+]]
  // CHECK: <label>:[[RESULT_B]]
  // CHECK-NEXT: br label %[[FINAL:.+]]
  case .B:
    return 11

  // CHECK: <label>:[[CASE_C]]
  // CHECK-NEXT: br label %[[RESULT_C:.+]]
  // CHECK: <label>:[[RESULT_C]]
  // CHECK-NEXT: br label %[[FINAL]]
  case .C:
    return 15

  // CHECK: <label>:[[CASE_A]]
  // CHECK-NEXT: br label %[[RESULT_A:.+]]
  // CHECK: <label>:[[RESULT_A]]
  // CHECK-NEXT: br label %[[FINAL]]
  case .A:
    return 10
  }

  // CHECK: <label>:[[FINAL]]
  // CHECK: %[[RETVAL:.+]] = phi i32 [ 10, %[[RESULT_A]] ], [ 15, %[[RESULT_C]] ], [ 11, %[[RESULT_B]] ]
  // CHECK: ret i32 %[[RETVAL]]
}
