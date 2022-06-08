// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

protocol P {}

struct S<T : P> {}

struct G<A, B> {}

extension G where A == S<B>, B : P {}

extension G where B : P, A == S<B> {}

extension G where B : P, A == S<B>, B : P {}
// expected-warning@-1 {{redundant conformance constraint 'B' : 'P'}}
