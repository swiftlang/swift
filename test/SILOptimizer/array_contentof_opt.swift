// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// This is an end-to-end test of the array(contentsOf) -> array(Element) optimization

// CHECK-LABEL: sil @{{.*}}testInt
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref @_T0Sa6appendyxFSi_Tg
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NEXT:   tuple
// CHECK-NEXT:   return
public func testInt(_ a: inout [Int]) {
  a += [1]
}

// CHECK-LABEL: sil @{{.*}}testThreeInt
// CHECK-NOT: apply
// CHECK:        [[FR:%[0-9]+]] = function_ref @_T0Sa15reserveCapacityySiFSi_Tg5
// CHECK-NEXT:   apply [[FR]]
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref @_T0Sa6appendyxFSi_Tg
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NEXT:   apply [[F]]
// CHECK-NEXT:   apply [[F]]
// CHECK-NEXT:   tuple
// CHECK-NEXT:   return
public func testThreeInts(_ a: inout [Int]) {
  a += [1, 2, 3]
}

// CHECK-LABEL: sil @{{.*}}testTooManyInts
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref  @_T0Sa6appendyqd__10contentsOf_t7ElementQyd__Rszs8SequenceRd__lFSi_SaySiGTg5
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NOT: apply
// CHECK:        return
public func testTooManyInts(_ a: inout [Int]) {
  a += [1, 2, 3, 4, 5, 6, 7]
}

// CHECK-LABEL: sil @{{.*}}testString
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref @_T0Sa6appendyxFSS_Tg
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NOT: apply
// CHECK:        tuple
// CHECK-NEXT:   return
public func testString(_ a: inout [String], s: String) {
  a += [s]
}

