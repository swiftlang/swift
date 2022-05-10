// RUN: %target-typecheck-verify-swift -requirement-machine-inferred-signatures=on -warn-redundant-requirements

protocol P1 {}
protocol P2 {}

struct Var<N> {}

extension Var : P2 where N : P1 { }

protocol P3 {}
extension Var : P3 where Self : P2 {} // expected-warning {{redundant conformance constraint 'Var<N>' : 'P2'}}
