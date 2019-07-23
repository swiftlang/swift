// RUN: %target-typecheck-verify-swift

protocol A {
  associatedtype V
  associatedtype E: Error

  init(value: V)
  init(error: E)

  func foo<U>(value: (V) -> U, error: (E) -> U) -> U
}

enum R<T, E: Error> : A {
  case foo(T)
  case bar(E)

  init(value: T) { self = .foo(value) }
  init(error: E) { self = .bar(error) }

  func foo<R>(value: (T) -> R, error: (E) -> R) -> R {
    fatalError()
  }
}

protocol P {
  associatedtype V

  @discardableResult
  func baz(callback: @escaping (V) -> Void) -> Self
}

class C<V> : P {
  func baz(callback: @escaping (V) -> Void) -> Self { return self }
}
class D<T, E: Error> : C<R<T, E>> {
  init(fn: (_ ret: @escaping (V) -> Void) -> Void) {}
}

extension A where V: P, V.V: A, E == V.V.E {
  func bar() -> D<V.V.V, V.V.E> {
    return D { complete in
      foo(value: { promise in promise.baz { result in } },
          error: { complete(R(error: $0)) })
    }
  }
}
