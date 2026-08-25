// Tests that a C++ class can conform to a Swift protocol.

// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=default
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=default -D VIRTUAL_METHODS

import ProtocolConformance

protocol HasReturn42 {
  mutating func return42() -> CInt // expected-note {{requires function 'return42()'}}
}

extension ConformsToProtocol : HasReturn42 {}

#if VIRTUAL_METHODS
extension HasVirtualMethod : HasReturn42 {}
#endif

extension DoesNotConformToProtocol : HasReturn42 {} // expected-error {{'DoesNotConformToProtocol' does not conform to protocol}} expected-note {{add stubs for conformance}}


protocol HasReturnNullable {
  mutating func returnPointer() -> UnsafePointer<Int32>?
}

// HasReturnNullable's returnNullable returns an implicitly unwrapped optional:
//   mutating func returnPointer() -> UnsafePointer<Int32>!
extension ReturnsNullableValue: HasReturnNullable {}

protocol HasReturnNonNull {
  mutating func returnPointer() -> UnsafePointer<Int32>
}

extension ReturnsNonNullValue: HasReturnNonNull {}


protocol Invertible {
  static prefix func !(obj: Self) -> Self
}

extension HasOperatorExclaim: Invertible {}

extension HasOperatorEqualEqual: @retroactive Equatable {}


protocol HasOperatorPlusEqualProtocol {
  static func +=(lhs: inout Self, x: Int32)
}

extension HasOperatorPlusEqualInt : HasOperatorPlusEqualProtocol {}

protocol HasOperatorCall {
  func callAsFunction(_ x: Int32) -> Int32
}

extension HasStaticOperatorCall : HasOperatorCall {}
