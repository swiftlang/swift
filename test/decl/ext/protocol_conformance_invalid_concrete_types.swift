// RUN: %target-typecheck-verify-swift

protocol P {}
class C1 {}
struct S {}
class C2 {}


// 1. Non-protocol type in conformance list
extension C2: S {}
// expected-error {{non-protocol type 'S' cannot be used in a conformance list}}


// 2. Class type in conformance list
extension C2: C1 {}
// expected-error {{non-protocol type 'C1' cannot be used in a conformance list}}


// 3. Class-constrained protocol composition in conformance list
extension C2: C1 & P {}
// expected-error {{protocol conformance list cannot contain concrete type 'C1'}}
