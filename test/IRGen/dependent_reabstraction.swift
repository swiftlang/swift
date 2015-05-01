// RUN: %target-swift-frontend -emit-ir %s | FileCheck %s

func markUsed<T>(t: T) {}

protocol A {
  typealias B
  func b(B)
}

struct X<Y> : A {
  // CHECK-LABEL: define hidden void @_TTWU__GV23dependent_reabstraction1XQ__S_1AS_FS1_1bUS1__U__fQPS1_FQS2_1BT_(%swift.type**, %V23dependent_reabstraction1X*, %swift.type* %Self)
  func b(b: X.Type) {
    let x: Any = b
    markUsed(b as X.Type)
  }
}

func foo<T: A>(x: T, _ y: T.B) {
  x.b(y)
}

let a = X<Int>()
let b = X<String>()

foo(a, X<Int>.self)
foo(b, X<String>.self)
