// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-sil %s | %FileCheck %s

import TypeClassification

// Make sure that "StructWithDestructor" is marked as non-trivial by checking for a
// "destroy_addr".
// CHECK-LABEL: @$s4main24testStructWithDestructoryyF
// CHECK: [[AS:%.*]] = alloc_stack $StructWithDestructor
// CHECK: destroy_addr [[AS]]
// CHECK-LABEL: end sil function '$s4main24testStructWithDestructoryyF'
public func testStructWithDestructor() {
  let d = StructWithDestructor()
}

// Make sure that "StructWithSubobjectDestructor" is marked as non-trivial by checking
// for a "destroy_addr".
// CHECK-LABEL: @$s4main33testStructWithSubobjectDestructoryyF
// CHECK: [[AS:%.*]] = alloc_stack $StructWithSubobjectDestructor
// CHECK: destroy_addr [[AS]]
// CHECK-LABEL: end sil function '$s4main33testStructWithSubobjectDestructoryyF'
public func testStructWithSubobjectDestructor() {
  let d = StructWithSubobjectDestructor()
}


