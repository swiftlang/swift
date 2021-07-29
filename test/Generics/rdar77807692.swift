// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T : Hashable
}

struct S1<Value> {}

extension S1 : P1 where Value : P1 {
  typealias T = Value.T
}

protocol P2 {
  associatedtype Value: P1
}

struct S2<X, Y: P2> where Y.Value == X {
  // CHECK-LABEL: Generic signature: <X, Y, T where X == S1<T>, Y : P2, T : P1, Y.Value == S1<T>>
  init<T>(_: T) where X == S1<T> { }
}
