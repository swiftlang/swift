// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx12 %s -swift-version 7
// REQUIRES: OS=macosx
// REQUIRES: swift7

protocol ProtoWithAssocType {
  associatedtype A // expected-note {{requirement 'A' declared here}}
}

struct WitnessSame: ProtoWithAssocType {
  @available(macOS 12, *)
  struct A {} // Ok, A is less available than its parent but available at the deployment target
}

struct WitnessTooNew: ProtoWithAssocType { // expected-error {{type 'WitnessTooNew' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS 13, *)
  struct A {} // expected-error {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 12 and newer}}
}

struct WitnessUnavailable: ProtoWithAssocType { // expected-error {{type 'WitnessUnavailable' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS, unavailable)
  struct A {} // expected-error {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'}}
}
