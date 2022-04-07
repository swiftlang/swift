// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on

protocol P1 {}
protocol P2 : P1 {}

protocol P3 {
  associatedtype A where A == S
}

struct S : P2 {}

func f1<T : P3>(_: T) where T.A : P1 {}
// expected-warning@-1 {{redundant conformance constraint 'T.A' : 'P1'}}

func f2<T : P3>(_: T) where T.A : P2 {}
// expected-warning@-1 {{redundant conformance constraint 'T.A' : 'P2'}}