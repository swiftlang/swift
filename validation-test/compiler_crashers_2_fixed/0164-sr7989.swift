// RUN: %target-typecheck-verify-swift

protocol P1 {}
protocol P2 {}

struct Var<N> {}

extension Var : P2 where N : P1 { }

protocol P3 {}
extension Var : P3 where Self : P2 {} // expected-error {{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'Var'?}}
