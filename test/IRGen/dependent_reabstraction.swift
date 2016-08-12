// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

func markUsed<T>(_ t: T) {}

protocol A {
  associatedtype B
  func b(_: B)
}

struct X<Y> : A {
  // CHECK-LABEL: define hidden void @_TTWurGV23dependent_reabstraction1Xx_S_1AS_FS1_1bfwx1BT_(%swift.type** noalias nocapture dereferenceable({{.*}}), %V23dependent_reabstraction1X* noalias nocapture, %swift.type* %Self, i8** %SelfWitnessTable)
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
