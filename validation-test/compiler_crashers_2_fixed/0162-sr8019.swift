// RUN: %target-typecheck-verify-swift

protocol P {}
struct A<C> {}
extension A: P where A: P {} // expected-error {{type 'A<C>' in conformance requirement does not refer to a generic parameter or associated type}}
