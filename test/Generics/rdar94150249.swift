// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype Value
}

protocol P2 {
  associatedtype Value
}

class G<Value> : P2 {}

protocol P3 {}

class C {}

extension P1 where Value: P2 {
  typealias Element = Value.Value

  // Make sure we can resolve 'Element' to 'V' on the left hand side of 'Element: P3'.

  // CHECK-LABEL: .P1 extension.set()@
  // CHECK-NEXT: Generic signature: <Self, V where Self : P1, V : C, V : P3, Self.[P1]Value == G<V>>
  func set<V>() where Element: P3 & C, Value == G<V> {
    takeP3(V.self)
  }
}

func takeP3<T : P3>(_: T.Type) {}
