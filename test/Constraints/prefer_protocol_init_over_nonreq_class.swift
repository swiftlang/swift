// RUN: %target-swift-frontend -import-objc-header %S/Inputs/simple_objc_class.h -swift-version 5 -emit-sil -verify %s | %FileCheck %s
// REQUIRES: objc_interop

protocol P {
  init()
}

protocol Q : P, Base {}

final class A : Base, Q {
  init() {
    super.init(name: "hello")
  }
}

class B {
  func foo() {
    let x: Q.Type = A.self
    // CHECK: witness_method {{.*}} Q, #P.init!allocator.1 : <Self where Self : P> (Self.Type) -> () -> Self
    bar(x.init())
  }

  func bar(_: Base) {}
}
