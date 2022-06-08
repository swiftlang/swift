// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

// A more complicated example.
protocol Equatable {}

struct Array<Element> {}

extension Array : Equatable where Element : Equatable {}

protocol Sequence {
  associatedtype Element
}

extension Array : Sequence {}

struct EquatableSequenceBox<T : Sequence> where T.Element : Equatable {
  // CHECK-LABEL: EquatableSequenceBox.withArrayArray@
  // CHECK-NEXT: Generic signature: <T, U where T == Array<Array<U>>, U : Equatable>
  func withArrayArray<U>(_: U) where T == Array<Array<U>> {}
}

// Make sure requirement desugaring handles conditional conformances.
protocol Hashable : Equatable {}

struct Set<Element : Hashable> {}

extension Array : Hashable where Element : Hashable {}

// CHECK-LABEL: doStuff@
// CHECK-NEXT: Generic signature: <U where U : Hashable>
func doStuff<U>(_: Set<Array<U>>) {}
