// Tests that a C++ class can conform to a Swift protocol.

// RUN: %target-swift-emit-silgen -I %S/Inputs -enable-cxx-interop %s

import ProtocolConformance

protocol HasReturn42 {
  mutating func return42() -> CInt
}

// FIXME:
// https://bugs.swift.org/browse/SR-12750
// SILGen currently hits an assertion failure in getParameterTypes() when the
// following protocol conformance is declared.
// extension ConformsToProtocol : HasReturn42 {}
