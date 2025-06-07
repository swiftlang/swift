// RUN: %target-swift-frontend -typecheck -verify %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct C {}

protocol P {
  associatedtype A
}

struct G<A>: P {}

struct Body<T: P> where T.A == C {
  // CHECK-LABEL: .init(_:)@
  // CHECK-NEXT: <T, U where T == G<C>, U : P, U.[P]A == C>
  init<U: P>(_: U) where T == G<T.A>, U.A == T.A {} 
}
