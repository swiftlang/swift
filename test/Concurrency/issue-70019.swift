// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-library-evolution

// REQUIRES: concurrency

// https://github.com/apple/swift/issues/70019

@usableFromInline
struct Foo { // expected-note {{consider making struct 'Foo' conform to the 'Sendable' protocol}}
  var integer: Int
}

struct Bar: Sendable {
  // FIXME: This warning should only be thrown in library evolution mode. If @usableFromInline is removed from Foo the Sendable conformance is synthesized as expected.
  var foo: Foo // expected-warning {{stored property 'foo' of 'Sendable'-conforming struct 'Bar' has non-Sendable type 'Foo'}}
}
