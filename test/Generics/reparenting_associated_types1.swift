// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

// REQUIRES: swift_feature_Reparenting

// This test ensures that 'BorrowingSeq.BSElement' cannot be witnessed by 'Seq.Element',
// even with a same-type requirement, because the string "BSElement" is earlier than "Element" lexicographically.

@reparentable
public protocol BorrowingSeq<BSElement> {
  associatedtype BSElement // expected-note {{protocol requires nested type 'BSElement'}}
}
public protocol Seq: BorrowingSeq {
  associatedtype Element
}
extension Seq: @reparented BorrowingSeq // expected-error {{type 'Self' does not conform to protocol 'BorrowingSeq'}} // expected-note {{add stubs for conformance}}
  where BSElement == Element {}


@reparentable
public protocol Door {
  associatedtype Key  // expected-note 2{{protocol requires nested type 'Key'}}
}
public protocol WitnessUsingSelf: Door {}
struct Wrapper<T> {}
extension WitnessUsingSelf: @reparented Door where Key == Wrapper<Key> {}
// expected-error@-1 {{cannot build rewrite system for generic signature; concrete type nesting limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[WitnessUsingSelf:Key].[concrete: Wrapper<Wrapper<Wrapper<Wrapper}}
// expected-error@-3 {{type 'Self' does not conform to protocol 'Door'}}
// expected-note@-4 {{add stubs for conformance}}



// Both A and Z are candidates to witness Key, so we pick neither!
public protocol AmbiguousWitness: Door {
  associatedtype A
  associatedtype Z
}
extension AmbiguousWitness: @reparented Door where A == Z, Z == Key {}
// expected-error@-1 {{type 'Self' does not conform to protocol 'Door'}}
// expected-note@-2 {{add stubs for conformance}}
