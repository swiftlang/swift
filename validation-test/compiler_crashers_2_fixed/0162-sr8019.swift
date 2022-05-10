// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

protocol P {}
struct A<C> {}
extension A: P where A: P {} // expected-warning {{redundant conformance constraint 'A<C>' : 'P'}}
