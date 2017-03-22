// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

public protocol A {}

public class AC : A{}

public class CVC<A1: AC> where A1: A {
  // CHECK-LABEL: define{{.*}} @_T021superclass_constraint3CVCCACyxGycfc
  public init() {
    // CHECK: [[A:%.*]] = alloca %T21superclass_constraint3CVCC*
    // CHECK-NOT: ret
    // CHECK: store %T21superclass_constraint3CVCC* %0, %T21superclass_constraint3CVCC** [[A]]
    // CHECK: ret
    var a = self
  }
}
