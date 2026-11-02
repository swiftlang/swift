// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx12 %s -swift-version 6 -verify-additional-prefix swift6-
// RUN: %if swift_future %{ %swift -typecheck -verify -target %target-cpu-apple-macosx12 %s -swift-version future -verify-additional-prefix future- %}
// REQUIRES: OS=macosx

protocol ProtoWithAssocType {
  associatedtype A // expected-note * {{requirement 'A' declared here}}
}

struct ConformsToProtoWithAssocType_WitnessOld: ProtoWithAssocType {
  @available(macOS 11, *)
  struct A {} // Ok, A is less available than its parent but more available than the deployment target
}

struct ConformsToProtoWithAssocType_WitnessSame: ProtoWithAssocType {
  @available(macOS 12, *)
  struct A {} // Ok, A is less available than its parent but available at the deployment target
}

struct ConformsToProtoWithAssocType_WitnessTooNew: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocType_WitnessTooNew' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS 13, *)
  struct A {} // expected-swift6-warning {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 12 and newer; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 12 and newer}}
}

struct ConformsToProtoWithAssocTypeInExtension_WitnessOld {}

extension ConformsToProtoWithAssocTypeInExtension_WitnessOld: ProtoWithAssocType {
  @available(macOS 11, *)
  struct A {} // Ok, A is less available than its parent but more available than the deployment target
}

struct ConformsToProtoWithAssocTypeInExtension_WitnessSame {}

extension ConformsToProtoWithAssocTypeInExtension_WitnessSame: ProtoWithAssocType {
  @available(macOS 12, *)
  struct A {} // Ok, A is less available than its parent but available at the deployment target
}

struct ConformsToProtoWithAssocTypeInExtension_WitnessTooNew {}

extension ConformsToProtoWithAssocTypeInExtension_WitnessTooNew: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocTypeInExtension_WitnessTooNew' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS 13, *)
  struct A {} // expected-swift6-warning {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 12 and newer; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 12 and newer}}
}

@available(macOS 13, *)
struct ConformsToProtoWithAssocType_NewerConformance: ProtoWithAssocType {
  struct A {} // Ok, A is as available as the conformance
}

struct ConformsToProtoWithAssocTypeInExtension_NewerConformance {}

@available(macOS 13, *)
extension ConformsToProtoWithAssocTypeInExtension_NewerConformance: ProtoWithAssocType {
  struct A {} // Ok, A is as available as the conformance
}

@available(macOS 13, *)
struct ConformsToProtoWithAssocType_NewerAndWitnessTooNew: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocType_NewerAndWitnessTooNew' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS 14, *)
  struct A {} // expected-swift6-warning {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 13 and newer; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{protocol 'ProtoWithAssocType' requires 'A' to be available in macOS 13 and newer}}
}

struct ConformsToProtoWithAssocType_WitnessUnavailable: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocType_WitnessUnavailable' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS, unavailable)
  struct A {} // expected-swift6-warning {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'}}
}

struct ConformsToProtoWithAssocType_WitnessUnavailableInExtension: ProtoWithAssocType {
  // expected-swift6-warning@-1 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'; this will be an error in a future Swift language mode}}
  // expected-future-error@-2 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'}}
  // expected-future-error@-3 {{type 'ConformsToProtoWithAssocType_WitnessUnavailableInExtension' does not conform to protocol 'ProtoWithAssocType'}}
}

extension ConformsToProtoWithAssocType_WitnessUnavailableInExtension {
  @available(macOS, unavailable)
  struct A {} // expected-note {{'A' declared here}}
}

struct ConformsToProtoWithAssocType_WitnessInUnavailableExtension: ProtoWithAssocType {
  // expected-swift6-warning@-1 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'; this will be an error in a future Swift language mode}}
  // expected-future-error@-2 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'}}
  // expected-future-error@-3 {{type 'ConformsToProtoWithAssocType_WitnessInUnavailableExtension' does not conform to protocol 'ProtoWithAssocType'}}
}

@available(macOS, unavailable)
extension ConformsToProtoWithAssocType_WitnessInUnavailableExtension {
  struct A {} // expected-note {{'A' declared here}}
}

struct ConformsToProtoWithAssocType_WitnessUnavailableMessage: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocType_WitnessUnavailableMessage' does not conform to protocol 'ProtoWithAssocType'}}
  @available(*, unavailable, message: "Just don't")
  struct A {} // expected-swift6-warning {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType': Just don't; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType': Just don't}}
}

struct ConformsToProtoWithAssocType_WitnessObsoleted: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocType_WitnessObsoleted' does not conform to protocol 'ProtoWithAssocType'}}
  @available(macOS, obsoleted: 11)
  struct A {} // expected-swift6-warning {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'}}
}

struct ConformsToProtoWithAssocType_WitnessIntroInSwift99: ProtoWithAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithAssocType_WitnessIntroInSwift99' does not conform to protocol 'ProtoWithAssocType'}}
  @available(swift, introduced: 99)
  struct A {} // expected-swift6-warning {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{unavailable struct 'A' was used to satisfy a requirement of protocol 'ProtoWithAssocType'}}
}

@available(macOS, unavailable)
struct ConformsToProtoWithAssocType_Unavailable: ProtoWithAssocType {
  struct A {} // Ok, the conformance is unavailable too.
}

@available(macOS, unavailable)
struct ConformsToProtoWithAssocType_WitnessAndConformanceUnavailable: ProtoWithAssocType {
  @available(macOS, unavailable)
  struct A {} // Ok, the conformance is unavailable too.
}

protocol ProtoWithNewAssocType {
  @available(macOS 13, *)
  associatedtype A
}

struct ConformsToProtoWithNewAssocType_WitnessOld: ProtoWithNewAssocType {
  struct A {} // Ok, A has always been available
}

struct ConformsToProtoWithNewAssocType_WitnessSame: ProtoWithNewAssocType {
  @available(macOS 13, *)
  struct A {} // Ok, A is as available as the associated type requirement
}

struct ConformsToProtoWithNewAssocType_WitnessTooNew: ProtoWithNewAssocType {
  // expected-future-error@-1 {{type 'ConformsToProtoWithNewAssocType_WitnessTooNew' does not conform to protocol 'ProtoWithNewAssocType'}}
  @available(macOS 14, *)
  struct A {} // expected-swift6-warning {{protocol 'ProtoWithNewAssocType' requires 'A' to be available in macOS 13 and newer; this will be an error in a future Swift language mode}}
  // expected-future-error@-1 {{protocol 'ProtoWithNewAssocType' requires 'A' to be available in macOS 13 and newer}}
}

protocol ProtoWithAssocTypeAndReq {
  associatedtype A

  @available(macOS 13, *)
  func req(_ a: A)
}

@available(macOS 11, *)
struct InferredOld {} // Ok, InferredOld is less available than its parent but more available than the deployment target

struct ConformsToProtoWithAssocTypeAndReq_InferredWitnessOld: ProtoWithAssocTypeAndReq {

  func req(_ a: InferredOld) {}
}

@available(macOS 12, *)
struct InferredSame {} // Ok, InferredSame is less available than its parent but available at the deployment target

struct ConformsToProtoWithAssocTypeAndReq_InferredWitnessSame: ProtoWithAssocTypeAndReq {
  func req(_ a: InferredSame) {}
}

@available(macOS 13, *)
struct InferredTooNew {} // expected-note {{'InferredTooNew' declared here}}

struct ConformsToProtoWithAssocTypeAndReq_InferredWitnessTooNew: ProtoWithAssocTypeAndReq {
  // expected-swift6-warning@-1 {{protocol 'ProtoWithAssocTypeAndReq' requires 'InferredTooNew' to be available in macOS 12 and newer; this will be an error in a future Swift language mode}}
  // expected-future-error@-2 {{protocol 'ProtoWithAssocTypeAndReq' requires 'InferredTooNew' to be available in macOS 12 and newer}}
  // expected-future-error@-3 {{type 'ConformsToProtoWithAssocTypeAndReq_InferredWitnessTooNew' does not conform to protocol 'ProtoWithAssocTypeAndReq'}}

  @available(macOS 13, *)
  func req(_ a: InferredTooNew) {}
}

@available(macOS, unavailable)
struct ConformsToProtoWithAssocTypeAndReq_InferredUnavailable: ProtoWithAssocTypeAndReq {
  @available(macOS, unavailable)
  struct InferredUnavailable {}

  func req(_ a: InferredUnavailable) {}
}
