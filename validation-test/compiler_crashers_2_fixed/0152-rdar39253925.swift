// RUN: %target-typecheck-verify-swift

struct S {}

protocol P {
  typealias A<T> = A_<T, Self>
}

struct A_<T, P> {}

extension S {
  subscript<T : P, U>(_: T, _: KeyPath<T, T.A<U>>) -> U {
    fatalError()
  }
}
