// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures -requirement-machine-inferred-signatures=verify %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures -requirement-machine-inferred-signatures=verify %s -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol Equatable {}

struct Array<Element> {}

extension Array : Equatable where Element : Equatable {}

struct EquatableBox<T : Equatable> {
  // CHECK: Generic signature: <T, U where T == Array<U>, U : Equatable>
  func withArray<U>(_: U) where T == Array<U> {}
}

// A conditional requirement with a protocol we haven't seen before.
protocol First {}

protocol Second {}

extension Array : First where Element : Second {}

struct SillyBox<T : First> {
  // CHECK: Generic signature: <T, U where T == Array<U>, U : Second>
  func withArray<U>(_: U) where T == Array<U> {}
}