// RUN: %target-swift-frontend -strict-concurrency=complete -emit-sil -o /dev/null %s -verify
// RUN: %target-swift-frontend -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -emit-sil -o /dev/null %s -verify

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

class C1 { } // expected-note{{class 'C1' does not conform to the 'Sendable' protocol}}
@_nonSendable class C2 { } // expected-note{{conformance of 'C2' to 'Sendable' has been explicitly marked unavailable here}}

protocol TestProtocol {
  associatedtype Value: Sendable
}

struct Test1: TestProtocol { // expected-warning{{type 'Test1.Value' (aka 'C1') does not conform to the 'Sendable' protocol}}
  typealias Value = C1
}

struct Test2: TestProtocol { // expected-warning{{conformance of 'C2' to 'Sendable' is unavailable}}
  // expected-note@-1{{in associated type 'Self.Value' (inferred as 'C2')}}
  typealias Value = C2
}
