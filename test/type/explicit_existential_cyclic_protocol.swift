// RUN: %target-typecheck-verify-swift -swift-version 5 -verify-additional-prefix no-explicit-any-
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-upcoming-feature ExistentialAny -verify-additional-prefix explicit-any-

// REQUIRES: swift_feature_ExistentialAny

// 'HasSelfOrAssociatedTypeRequirementsRequest' should evaluate to false in
// the event of a cycle because we will have considered all the protocols in a
// cyclic hierarchy by the time the cycle is hit.
do {
  do {
    protocol P1 : P2 {}
    // expected-no-explicit-any-note@-1 2 {{protocol 'P1' declared here}}
    // expected-explicit-any-note@-2 1 {{protocol 'P1' declared here}}
    protocol P2 : P1 {}
    // expected-no-explicit-any-error@-1 2 {{protocol 'P2' refines itself}}
    // expected-explicit-any-error@-2 1 {{protocol 'P2' refines itself}}

    // Diagnosed only with the feature enabled, as a protocol without
    // "HasSelfOrAssociatedTypeRequirements" should.
    let _: P2
    // expected-explicit-any-warning@-1 {{use of protocol 'P2' as a type must be written 'any P2'}}
  }
  do {
    protocol P0 { associatedtype A }
    protocol P1 : P2, P0 {}
    // expected-no-explicit-any-note@-1 2 {{protocol 'P1' declared here}}
    // expected-explicit-any-note@-2 1 {{protocol 'P1' declared here}}
    protocol P2 : P1 {}
    // expected-no-explicit-any-error@-1 2 {{protocol 'P2' refines itself}}
    // expected-explicit-any-error@-2 1 {{protocol 'P2' refines itself}}

    let _: P2
    // expected-warning@-1 {{use of protocol 'P2' as a type must be written 'any P2'}}
  }
}
