// RUN: %target-swift-frontend %use_no_opaque_pointers %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir

public protocol A {}

public class AC : A{}

public class CVC<A1: AC> where A1: A {
  // CHECK-LABEL: define{{.*}} @"$s21superclass_constraint3CVCCACyxGycfc"
  public init() {
    // CHECK: [[A:%.*]] = alloca %T21superclass_constraint3CVCC*
    // CHECK-NOT: ret %T21superclass_constraint3CVCC*
    // CHECK: store %T21superclass_constraint3CVCC* %0, %T21superclass_constraint3CVCC** [[A]]
    // CHECK: ret %T21superclass_constraint3CVCC*
    var a = self
  }
}
