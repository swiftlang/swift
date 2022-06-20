// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1  {
  associatedtype T: P2 where T.T == Self
}

protocol P2 {
  associatedtype T
}

protocol P3: P2 {}

protocol P4 {
  associatedtype T
}

// CHECK-LABEL: .G@
// CHECK-NEXT: Generic signature: <T where T : P4, T.[P4]T : P1, T.[P4]T.[P1]T : P3>
class G<T: P4> where T.T: P1, T.T.T: P3 {}
