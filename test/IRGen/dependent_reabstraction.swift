// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

func markUsed<T>(_ t: T) {}

protocol A {
  associatedtype B
  func b(_: B)
}

struct X<Y> : A {
  // CHECK-LABEL: define internal swiftcc void @"$s23dependent_reabstraction1XVyxGAA1AA2aEP1byy1BQzFTW"(%swift.type** noalias nocapture dereferenceable({{.*}}) %0, %T23dependent_reabstraction1XV* noalias nocapture swiftself %1, %swift.type* %Self, i8** %SelfWitnessTable)
  func b(_ b: X.Type) {
    let x: Any = b
    markUsed(b as X.Type)
  }
}

func foo<T: A>(_ x: T, _ y: T.B) {
  x.b(y)
}

let a = X<Int>()
let b = X<String>()

foo(a, X<Int>.self)
foo(b, X<String>.self)
