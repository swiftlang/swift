// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/SendableConformances.swiftmodule -module-name SendableConformances %S/Inputs/SendableConformances.swift

// RUN: %target-swift-frontend -typecheck %s -verify -swift-version 6 -I %t

// REQUIRES: concurrency

import SendableConformances

typealias RequireSendable<T: Sendable> = T

extension NonSendableClass: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{conformance of 'NonSendableClass' to protocol 'Sendable' was already stated in the type's module 'SendableConformances'}}

typealias CheckNonSendableClass = RequireSendable<NonSendableClass>

extension SendableStruct: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{conformance of 'SendableStruct' to protocol 'Sendable' was already stated in the type's module 'SendableConformances'}}

@available(*, unavailable)
extension AnotherSendableStruct: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{conformance of 'AnotherSendableStruct' to protocol 'Sendable' was already stated in the type's module 'SendableConformances'}}

typealias CheckAnotherSendableStruct = RequireSendable<AnotherSendableStruct>
