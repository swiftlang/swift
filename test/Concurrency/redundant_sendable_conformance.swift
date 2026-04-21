// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/SendableConformances.swiftmodule -module-name SendableConformances %S/Inputs/SendableConformances.swift

// RUN: %target-swift-frontend -typecheck %s -verify -verify-ignore-unrelated -swift-version 6 -I %t

// REQUIRES: concurrency

import SendableConformances

typealias RequireSendable<T: Sendable> = T

extension SendableStruct: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{conformance of 'SendableStruct' to protocol 'Sendable' was already stated in the type's module 'SendableConformances'}}
typealias CheckSendableStruct = RequireSendable<SendableStruct>

@available(*, unavailable)
extension AnotherSendableStruct: @retroactive @unchecked Sendable {}
// expected-warning@-1 {{conformance of 'AnotherSendableStruct' to protocol 'Sendable' was already stated in the type's module 'SendableConformances'}}

typealias CheckAnotherSendableStruct = RequireSendable<AnotherSendableStruct>
