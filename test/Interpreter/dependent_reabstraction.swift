// RUN: %target-run-simple-swift

protocol A {
  typealias B
  func b(B)
}

struct X<Y> : A {
  func b(b: X.Type) {
    let x: Any = b
    println(b as X.Type)
  }
}

func foo<T: A>(x: T, _ y: T.B) {
  x.b(y)
}

let a = X<Int>()
let b = X<String>()

// CHECK: (Metatype)
foo(a, X<Int>.self)
// CHECK-NEXT: (Metatype)
foo(b, X<String>.self)
