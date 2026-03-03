// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// Originally from rdar://35088384:
//
// This used to crash in Xcode 9 GM, and fails with a diagnostic in more
// recent swift-4.0-branch builds, because we incorrectly infer the type
// of the array literal as [Any].

do {
  protocol Command {}

  struct Cut: Command {}
  struct Copy: Command {}
  struct Paste: Command {}

  let _ = Array<any Command>([Cut(), Copy(), Paste()])
  let _ = Array<(any Command)?>([Cut(), Copy(), Paste()])
  let _ = Array<any Command.Type>([Cut.self, Copy.self, Paste.self])
  let _ = Array<(any Command.Type)?>([Cut.self, Copy.self, Paste.self])
}

// This expression first appeared in test/embedded/dict-init.swift.
// Test some variations.
do {
  let _ = Dictionary(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, String>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, String>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, StaticString>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, StaticString>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])

  let _ = Dictionary.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, String>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, String>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, StaticString>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, StaticString>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])

  let _: Dictionary = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Int, String> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Double, String> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Int, StaticString> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Double, StaticString> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
}

// rdar://problem/38159133
// https://github.com/apple/swift/issues/49673
// Swift 4.1 Xcode 9.3b4 regression

protocol P_38159133 {}

do {
  class Super {}
  class A: Super, P_38159133 {}
  class B: Super, P_38159133 {}

  func rdar38159133(_ a: A?, _ b: B?) {
    let _: [P_38159133] = [a, b].compactMap { $0 } // Ok
  }
}

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
    init(_: () -> ()) where E == Never {}
    init(_: () throws -> ()) where E == Error {}
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
      f2(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
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
