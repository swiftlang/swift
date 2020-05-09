// RUN: %target-typecheck-verify-swift

protocol P1 {}
protocol P2 {}

struct Var<N> {}

extension Var : P2 where N : P1 { }

protocol P3 {}
extension Var : P3 where Self : P2 {} // expected-error {{type 'Var<N>' in conformance requirement does not refer to a generic parameter or associated type}}
