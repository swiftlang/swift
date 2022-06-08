// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]X == _A<Int>>
protocol P {
  typealias A = _A
  typealias B = A<Int>

  associatedtype X where X == B
}

struct _A<T> {}
