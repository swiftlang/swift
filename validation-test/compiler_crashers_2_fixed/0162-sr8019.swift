// RUN: %target-typecheck-verify-swift -requirement-machine-inferred-signatures=on

protocol P {}
struct A<C> {}
extension A: P where A: P {} // expected-warning {{redundant conformance constraint 'A<C>' : 'P'}}
