// RUN: %swift -emit-ir %s | FileCheck %s

protocol A {
  typealias B
  func b(B)
}

struct X<Y> : A {
  // CHECK-LABEL: define void @_TTWV23dependent_reabstraction1XS_1AFS1_1bUS1__U__fRQPS1_FQS2_1BT_(%swift.type** noalias, %V23dependent_reabstraction1X* noalias, %swift.type* %Self)
  func b(b: X.Type) {
    let x: Any = b
    println(b as X.Type)
  }
}

func foo<T: A>(x: T, y: T.B) {
  x.b(y)
}

let a = X<Int>()
let b = X<String>()

foo(a, X<Int>.self)
foo(b, X<String>.self)
