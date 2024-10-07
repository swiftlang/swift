// RUN: %target-typecheck-verify-swift -I %S/Inputs -swift-version 6 -cxx-interoperability-mode=upcoming-swift

import Sendable // expected-warning {{add '@preconcurrency' to treat 'Sendable'-related errors from module 'Sendable' as warnings}}

func takesSendable<T: Sendable>(_ x: T.Type) {}

takesSendable(HasPrivatePointerField.self) // expected-error {{type 'HasPrivatePointerField' does not conform to the 'Sendable' protocol}}
takesSendable(HasProtectedPointerField.self) // expected-error {{type 'HasProtectedPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(HasPublicPointerField.self) // expected-error {{type 'HasPublicPointerField' does not conform to the 'Sendable' protocol}}

takesSendable(HasPrivateNonSendableField.self) // expected-error {{type 'HasPrivateNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(HasProtectedNonSendableField.self) // expected-error {{type 'HasProtectedNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(HasPublicNonSendableField.self) // expected-error {{type 'HasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
