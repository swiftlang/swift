// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P {
  associatedtype T : Q
}

protocol Q {
  associatedtype U
}

struct S<T : Q> : P {}

// Make sure concrete contraction can transform X.T.U => S<Y>.T.U => Y.U
struct G<X : P, Y : Q> where X.T.U == Int {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y where X == S<Y>, Y : Q, Y.[Q]U == Int>
extension G where X == S<Y> {}
