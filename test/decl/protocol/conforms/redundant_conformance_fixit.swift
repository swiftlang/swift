// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/47634
// The redundant conformance diagnostic should offer to remove the redundant
// conformance from the inheritance clause.

protocol P1 {}
protocol P2 {}

// Remove the whole inheritance clause when the redundant conformance is its
// only entry.
struct S1: P1 {} // expected-note {{'S1' declares conformance to protocol 'P1' here}}
extension S1: P1 {} // expected-error {{redundant conformance of 'S1' to protocol 'P1'}} {{13-17=}}

// Remove through the start of the next entry when the redundant conformance
// comes first.
struct S2: P1 {} // expected-note {{'S2' declares conformance to protocol 'P1' here}}
extension S2: P1, P2 {} // expected-error {{redundant conformance of 'S2' to protocol 'P1'}} {{15-19=}}

// Remove from the end of the previous entry when the redundant conformance
// comes last.
struct S3: P1 {} // expected-note {{'S3' declares conformance to protocol 'P1' here}}
extension S3: P2, P1 {} // expected-error {{redundant conformance of 'S3' to protocol 'P1'}} {{17-21=}}

// The conformance on the type declaration itself wins even when the
// extension comes first in the file; the extension gets the Fix-It.
extension S4: P1 {}
// expected-error@-1 {{redundant conformance of 'S4' to protocol 'P1'}} {{13-17=}}
struct S4: P1 {} // expected-note {{'S4' declares conformance to protocol 'P1' here}}

// The Fix-It also applies to the type declaration's own inheritance clause,
// and entries surrounding the removed one are kept intact.
class Base2: P1 {}
class Sub2: Base2, P1 {}
// expected-error@-1 {{redundant conformance of 'Sub2' to protocol 'P1'}} {{18-22=}}
// expected-note@-2 {{'Sub2' inherits conformance to protocol 'P1' from superclass here}}

// No Fix-It when the conformance was written as part of a protocol
// composition; the other members of the composition must stay.
struct S5: P1 {} // expected-note {{'S5' declares conformance to protocol 'P1' here}}
extension S5: P1 & P2 {} // expected-error {{redundant conformance of 'S5' to protocol 'P1'}} {{none}}

// No Fix-It when the conformance came from a typealias that also states
// other conformances.
typealias BothP1AndP2 = P1 & P2
struct S6: P1 {} // expected-note {{'S6' declares conformance to protocol 'P1' here}}
extension S6: BothP1AndP2 {} // expected-error {{redundant conformance of 'S6' to protocol 'P1'}} {{none}}

// A typealias naming just the redundant protocol can be removed.
typealias JustP1 = P1
struct S7: P1 {} // expected-note {{'S7' declares conformance to protocol 'P1' here}}
extension S7: JustP1 {} // expected-error {{redundant conformance of 'S7' to protocol 'P1'}} {{13-21=}}
