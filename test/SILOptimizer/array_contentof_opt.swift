// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// This is an end-to-end test of the array(contentsOf) -> array(Element) optimization

// CHECK-LABEL: sil @{{.*}}testInt
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref @$SSa6appendyyxFSi_Tg5
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NEXT:   tuple
// CHECK-NEXT:   return
public func testInt(_ a: inout [Int]) {
  a += [1]
}

// CHECK-LABEL: sil @{{.*}}testThreeInt
// CHECK-NOT: apply
// CHECK:        [[FR:%[0-9]+]] = function_ref @$SSa15reserveCapacityyySiFSi_Tg5
// CHECK-NEXT:   apply [[FR]]
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref @$SSa6appendyyxFSi_Tg5
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
// CHECK:        [[F:%[0-9]+]] = function_ref  @$SSa6append10contentsOfyqd___t7ElementQyd__RszSTRd__lFSi_SaySiGTg5
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NOT: apply
// CHECK:        return
public func testTooManyInts(_ a: inout [Int]) {
  a += [1, 2, 3, 4, 5, 6, 7]
}

// CHECK-LABEL: sil @{{.*}}testString
// CHECK-NOT: apply
// CHECK:        [[F:%[0-9]+]] = function_ref @$SSa6appendyyxFSS_Tg5
// CHECK-NOT: apply
// CHECK:        apply [[F]]
// CHECK-NOT: apply
// CHECK:        tuple
// CHECK-NEXT:   return
public func testString(_ a: inout [String], s: String) {
  a += [s]
}

// This is not supported yet. Just check that we don't crash on this.`
public func dontPropagateContiguousArray(_ a: inout ContiguousArray<UInt8>) {
  a += [4]
}
