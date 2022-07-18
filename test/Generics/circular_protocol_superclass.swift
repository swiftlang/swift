// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: circular_protocol_superclass.(file).A@
// CHECK-NEXT: Requirement signature: <Self where Self : a>
protocol A : a {
  associatedtype e : a
}

class a : A {
  typealias e = a
}
