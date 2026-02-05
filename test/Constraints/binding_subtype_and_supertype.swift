// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// Tests for a special form of inference where we have both a
// subtype and a supertype binding for a type variable, and the
// subtype binding contains a type variable but the supertype
// does not. We propagate the more concrete type from the
// supertype binding to the subtype binding in this case.
//
// See BindingSet::addBinding() for details.

// FIXME: This needs more tests.

do {
  func f<R>(fn: () -> [R]) -> [R] { [] }

  // Requires collection upcast from Array<(key: String, value: String)> to `Array<(String, String)>`
  func g(v: [String: String]) {
    let _: [(String, String)] = f { return Array(v) } + v
    let _: [(String, String)] = v + f { return Array(v) }
    let _: [(String, String)] = f { return Array(v) } + Array(v)
    let _: [(String, String)] = Array(v) + f { return Array(v) }
    let _: [(String, String)] = v + Array(v)
    let _: [(String, String)] = Array(v) + v
    let _: [(String, String)] = Array(v) + Array(v)
  }
}

// https://github.com/swiftlang/swift/issues/77003
do {
  struct Task<E> {
    init(_: () -> ()) where E == Never {}  // expected-note {{found this candidate}}
    init(_: () throws -> ()) where E == Error {}  // expected-note {{found this candidate}}
  }

  func f0<T, U>(_: T.Type, _ fn: (T) -> U, _: (U) -> ()) {}

  func f1<T, U>(_: T.Type, _ fn: (T) -> U?, _: (U) -> ()) {}

  func g1(x: Int?.Type) {
      f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
      f1(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  }

  func f2<T, U>(_: T.Type, _ fn: (T) -> (U, U), _: (U) -> ()) {}

  // FIXME
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
}
