// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol Equatable {}

struct Array<Element> {}

extension Array : Equatable where Element : Equatable {}

struct EquatableBox<T : Equatable> {
  // CHECK: Generic signature: <T, U where T == Array<U>, U : Equatable>
  func withArray<U>(_: U) where T == Array<U> {}
}

// A conditional requirement with a protocol we haven't seen before.
protocol First {}

protocol Second {
  associatedtype X
  associatedtype Y where X == Y
}

extension Array : First where Element : Second {}

func sameType<T>(_: T.Type, _: T.Type) {}

struct SillyBox<T : First> {
  // Make sure we pick up the same-type requirement from 'Second' -- U.Y should
  // minimize down to U.X.

  // CHECK: Generic signature: <T, U, V where T == Array<U>, U : Second, V == U.[Second]X>
  func withArray<U, V>(_: U) where T == Array<U>, V == U.Y {
    sameType(U.X.self, U.Y.self)
  }
}
