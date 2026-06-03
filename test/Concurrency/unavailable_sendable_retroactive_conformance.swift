// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/SendableConformances.swiftmodule -module-name SendableConformances %S/Inputs/SendableConformances.swift

// RUN: %target-swift-frontend -typecheck %s -verify -swift-version 6 -I %t -verify-additional-file SendableConformances.NonSendableClass -verify-additional-file SendableConformances.NonSendableViaProtocol -verify-additional-file SendableConformances.NonSendableChild -verify-additional-file SendableConformances.NonSendableViaAttr -verify-additional-file SendableConformances.NonSendableViaComposition

// REQUIRES: concurrency

import SendableConformances

typealias RequireSendable<T: Sendable> = T

extension NonSendableClass: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{'NonSendableClass' was declared with an unavailable 'Sendable' conformance in 'SendableConformances'; conforming here risks data races}}
// expected-note@SendableConformances.NonSendableClass:2 {{'NonSendableClass' declares unavailable conformance to protocol 'Sendable' here}}
typealias CheckNonSendableClass = RequireSendable<NonSendableClass>

extension NonSendableViaProtocol: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{'NonSendableViaProtocol' was declared with an unavailable 'Sendable' conformance in 'SendableConformances'; conforming here risks data races}}
// expected-note@SendableConformances.NonSendableViaProtocol:2 {{'NonSendableViaProtocol' declares unavailable conformance to protocol 'Sendable' here}}
typealias CheckNonSendableViaProtocol = RequireSendable<NonSendableViaProtocol>

extension NonSendableViaAttr: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{'NonSendableViaAttr' was declared with an unavailable 'Sendable' conformance in 'SendableConformances'; conforming here risks data races}}
// expected-note@SendableConformances.NonSendableViaAttr:2 {{'NonSendableViaAttr' declares unavailable conformance to protocol 'Sendable' here}}
typealias CheckNonSendableViaAttr = RequireSendable<NonSendableViaAttr>

extension NonSendableChild: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{'NonSendableChild' inherits an unavailable 'Sendable' conformance; conforming here risks data races}}
// expected-note@SendableConformances.NonSendableChild:1 {{'NonSendableChild' inherits unavailable conformance to protocol 'Sendable' from superclass here}}

extension NonSendableViaComposition: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{'NonSendableViaComposition' was declared with an unavailable 'Sendable' conformance in 'SendableConformances'; conforming here risks data races}}
// expected-note@SendableConformances.NonSendableViaComposition:2 {{'NonSendableViaComposition' declares unavailable conformance to protocol 'Sendable' here}}
