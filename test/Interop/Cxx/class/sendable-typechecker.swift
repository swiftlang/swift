// RUN: %target-typecheck-verify-swift -I %S/Inputs -swift-version 6 -cxx-interoperability-mode=upcoming-swift

import Sendable // expected-warning {{add '@preconcurrency' to treat 'Sendable'-related errors from module 'Sendable' as warnings}}

func takesSendable<T: Sendable>(_ x: T.Type) {}

takesSendable(HasPrivatePointerField.self) // expected-error {{type 'HasPrivatePointerField' does not conform to the 'Sendable' protocol}}
takesSendable(HasProtectedPointerField.self) // expected-error {{type 'HasProtectedPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(HasPublicPointerField.self) // expected-error {{type 'HasPublicPointerField' does not conform to the 'Sendable' protocol}}

takesSendable(HasPrivateNonSendableField.self) // expected-error {{type 'HasPrivateNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(HasProtectedNonSendableField.self) // expected-error {{type 'HasProtectedNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(HasPublicNonSendableField.self) // expected-error {{type 'HasPublicNonSendableField' does not conform to the 'Sendable' protocol}}

takesSendable(DerivedFromHasPublicPointerField.self) // expected-error {{type 'DerivedFromHasPublicPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedFromHasPublicNonSendableField.self) // expected-error {{type 'DerivedFromHasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedFromHasPrivatePointerField.self) // expected-error {{type 'DerivedFromHasPrivatePointerField' does not conform to the 'Sendable' protocol}}

takesSendable(DerivedPrivatelyFromHasPublicPointerField.self) // expected-error {{type 'DerivedPrivatelyFromHasPublicPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedPrivatelyFromHasPublicNonSendableField.self) // expected-error {{type 'DerivedPrivatelyFromHasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedPrivatelyFromHasPrivatePointerField.self) // expected-error {{type 'DerivedPrivatelyFromHasPrivatePointerField' does not conform to the 'Sendable' protocol}}

takesSendable(DerivedProtectedFromHasPublicPointerField.self) // expected-error {{type 'DerivedProtectedFromHasPublicPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedProtectedFromHasPublicNonSendableField.self) // expected-error {{type 'DerivedProtectedFromHasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedProtectedFromHasPrivatePointerField.self) // expected-error {{type 'DerivedProtectedFromHasPrivatePointerField' does not conform to the 'Sendable' protocol}}
