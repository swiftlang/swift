// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency
// REQUIRES: OS=macosx

@available(SwiftStdlib 5.1, *)
struct NS1 { }

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension NS1: Sendable { }
// expected-note@-1 2{{conformance of 'NS1' to 'Sendable' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
struct NS2 { // expected-note{{consider making struct 'NS2' conform to the 'Sendable' protocol}}
  var ns1: NS1
}

@available(SwiftStdlib 5.1, *)
struct NS3 { }

@available(SwiftStdlib 5.3, *)
extension NS3: Sendable { }

@available(SwiftStdlib 5.1, *)
func acceptCV<T: Sendable>(_: T) { }

@available(SwiftStdlib 5.1, *)
func testCV(ns1: NS1, ns1array: [NS1], ns2: NS2, ns3: NS3) {
  acceptCV(ns1)
  acceptCV(ns1array)
  acceptCV(ns2)
  acceptCV(ns3)
}

@available(SwiftStdlib 5.1, *)
func testCV(ns1: NS1, ns1array: [NS1], ns2: NS2, ns3: NS3) async {
  acceptCV(ns1) // expected-warning{{conformance of 'NS1' to 'Sendable' is unavailable}}
  acceptCV(ns1array) // expected-warning{{conformance of 'NS1' to 'Sendable' is unavailable}}
  acceptCV(ns2) // expected-warning{{type 'NS2' does not conform to the 'Sendable' protocol}}
  acceptCV(ns3) // expected-warning{{conformance of 'NS3' to 'Sendable' is only available in macOS 11.0 or newer}}
  // expected-note@-1{{add 'if #available' version check}}
}
