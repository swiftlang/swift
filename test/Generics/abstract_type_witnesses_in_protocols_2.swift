// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype X
}

protocol P2 {
  associatedtype Y
}

protocol P3: P2 {
  associatedtype Z: P1 where Z.X == C
}

struct C: P3 {
  typealias Y = C

  struct Z: P1 {
    typealias X = C
  }
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=P1
// CHECK-NEXT: Generic signature: <Self where Self : P1, Self.[P1]X : P2, Self.[P1]X.[P2]Y == C>
extension P1 where X: P2, X.Y : P3, X.Y == C {
  func foo(_ x: X.Y) -> C { return x }
}
