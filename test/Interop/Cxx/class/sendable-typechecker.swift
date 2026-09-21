// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -swift-version 6 -cxx-interoperability-mode=upcoming-swift

import Sendable // expected-warning {{add '@preconcurrency' to treat 'Sendable'-related errors from module 'Sendable' as warnings}}

func takesSendable<T: Sendable>(_ x: T.Type) {}
func takesSendable<T>(_: T.Type) where T: Sendable, T: ~Copyable {}
func takesSendable(_: @Sendable () -> Void) {}

takesSendable(HasPrivatePointerField.self) // expected-warning {{type 'HasPrivatePointerField' does not conform to the 'Sendable' protocol}}
takesSendable(HasProtectedPointerField.self) // expected-warning {{type 'HasProtectedPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(HasPublicPointerField.self) // expected-warning {{type 'HasPublicPointerField' does not conform to the 'Sendable' protocol}}

takesSendable(HasPrivateNonSendableField.self) // expected-warning {{type 'HasPrivateNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(HasProtectedNonSendableField.self) // expected-warning {{type 'HasProtectedNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(HasPublicNonSendableField.self) // expected-warning {{type 'HasPublicNonSendableField' does not conform to the 'Sendable' protocol}}

takesSendable(DerivedFromHasPublicPointerField.self) // expected-warning {{type 'DerivedFromHasPublicPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedFromHasPublicNonSendableField.self) // expected-warning {{type 'DerivedFromHasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedFromHasPrivatePointerField.self) // expected-warning {{type 'DerivedFromHasPrivatePointerField' does not conform to the 'Sendable' protocol}}

takesSendable(DerivedPrivatelyFromHasPublicPointerField.self) // expected-warning {{type 'DerivedPrivatelyFromHasPublicPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedPrivatelyFromHasPublicNonSendableField.self) // expected-warning {{type 'DerivedPrivatelyFromHasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedPrivatelyFromHasPrivatePointerField.self) // expected-warning {{type 'DerivedPrivatelyFromHasPrivatePointerField' does not conform to the 'Sendable' protocol}}

takesSendable(DerivedProtectedFromHasPublicPointerField.self) // expected-warning {{type 'DerivedProtectedFromHasPublicPointerField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedProtectedFromHasPublicNonSendableField.self) // expected-warning {{type 'DerivedProtectedFromHasPublicNonSendableField' does not conform to the 'Sendable' protocol}}
takesSendable(DerivedProtectedFromHasPrivatePointerField.self) // expected-warning {{type 'DerivedProtectedFromHasPrivatePointerField' does not conform to the 'Sendable' protocol}}

func checkSendableRef(k: borrowing SendableKlass) {
  takesSendable(SendableKlass.self) // Ok
  takesSendable(k.test) // Ok
}
