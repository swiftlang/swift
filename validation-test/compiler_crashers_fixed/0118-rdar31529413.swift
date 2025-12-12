// RUN: %target-typecheck-verify-swift

enum E { case A }

class C<T> {
  struct Nested {
    var value: T
  }
}

class D: C<E> {
  func test() {
    _ = Nested(value: .A)
  }
}
