// Tests that a C++ class can conform to a Swift protocol.

// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import ProtocolConformance

protocol HasReturn42 {
  mutating func return42() -> CInt // expected-note {{requires function 'return42()'}}
}

extension ConformsToProtocol : HasReturn42 {}

extension DoesNotConformToProtocol : HasReturn42 {} // expected-error {{'DoesNotConformToProtocol' does not conform to protocol}}
