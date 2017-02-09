// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

public protocol A {}

public class AC : A{}

public class CVC<A1: AC> where A1: A {
  // CHECK-LABEL: define{{.*}} @_T021superclass_constraint3CVCCACyxGycfc
  public init() {
    // CHECK: [[A:%.*]] = alloca %C21superclass_constraint3CVC*
    // CHECK-NOT: ret
    // CHECK: store %C21superclass_constraint3CVC* %0, %C21superclass_constraint3CVC** [[A]]
    // CHECK: ret
    var a = self
  }
}
