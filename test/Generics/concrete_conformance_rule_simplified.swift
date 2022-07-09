// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol P1 {}

protocol P2 {
  associatedtype T: P1
}

protocol P3: P1 {}

protocol P4: P3 {}

protocol P5: P2 {
  associatedtype U: P5 where U.T: P4
}

protocol P6: P1 {
  associatedtype V: P6 & P4
}

struct C: P6 & P4 {
  typealias V = C
}

struct G1<T: P6>: P5 {
  typealias U = G1<T.V>
}

struct G2<T: P5> where T.T: P4 {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G2
// CHECK-NEXT: Generic signature: <T where T == G1<C>>
extension G2 where T == G1<C> {}
