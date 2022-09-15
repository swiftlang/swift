// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

// https://github.com/apple/swift/issues/50522

protocol P1 {}
protocol P2 {}

struct Var<N> {}

extension Var : P2 where N : P1 { }

protocol P3 {}
extension Var : P3 where Self : P2 {} // expected-warning {{redundant conformance constraint 'Var<N>' : 'P2'}}
