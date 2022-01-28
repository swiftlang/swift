// RUN: %target-typecheck-verify-swift -requirement-machine-inferred-signatures=verify
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures -requirement-machine-inferred-signatures=verify %s 2>&1 | %FileCheck %s

// A more complicated example.
protocol Equatable {}

struct Array<Element> {}

extension Array : Equatable where Element : Equatable {}

protocol Sequence {
  associatedtype Element
}

extension Array : Sequence {}

struct EquatableSequenceBox<T : Sequence> where T.Element : Equatable {
  // CHECK: Generic signature: <T, U where T == Array<Array<U>>, U : Equatable>
  func withArrayArray<U>(_: U) where T == Array<Array<U>> {}
}