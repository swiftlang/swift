// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -solver-enable-diagnose-valid-salvage  -solver-disable-splitter
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -solver-enable-diagnose-valid-salvage

// https://github.com/swiftlang/swift/issues/77003
struct Task<E> {
  init(_: () -> ()) where E == Never {} // expected-note 3 {{found this candidate}}
  init(_: () throws -> ()) where E == Error {} // expected-note 3 {{found this candidate}}
}

func f0<T, U>(_: T.Type, _ fn: (T) -> U, _: (U) -> ()) {}

func f1<T, U>(_: T.Type, _ fn: (T) -> U?, _: (U) -> ()) {}

func g1(x: Int?.Type) {
  f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  f1(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
}

func f2<T, U>(_: T.Type, _ fn: (T) -> (U, U), _: (U) -> ()) {}

func g2(x: (Int, Int).Type) {
  f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  f2(x, { $0 }, { _ in Task {} })  // expected-error {{ambiguous use of 'init(_:)'}}
}

func f3<T, U>(_: T.Type, _ fn: (T) -> [U], _: (U) -> ()) {}

func g3(x: [Int].Type) {
  f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  f3(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
}

func f4<T, U>(_: T.Type, _ fn: (T) -> [U?], _: (U) -> ()) {}

func g4(x: [Int?].Type) {
  f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  f4(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
}


class C {}
class D<X>: C {}

func foo<T, U, V, X, Y>(_: T.Type, _ fn: (T) -> D<U>, _: V.Type, _ fn2: (V) -> (X, X), _: (X) -> Y) {}

func bar(_ x: D<Int>.Type, _ y: (Int, Int).Type) {
  foo(x, { $0 }, y, { $0 }, { _ in Task {} }) // expected-error {{ambiguous use of 'init(_:)'}}
}

func bar2<T, U, V>(_: T.Type, _ fn: (T) -> (U, U), _: (U) -> V) {}
func foo2(_ x: (Int, Int).Type) {
  bar2(x, { $0 }, { _ in Task {} }) // expected-error {{ambiguous use of 'init(_:)'}}
}
