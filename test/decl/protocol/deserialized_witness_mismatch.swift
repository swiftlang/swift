// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/deserialized_witness_mismatch_other.swift -emit-module-path %t/deserialized_witness_mismatch_other.swiftmodule
// RUN: %target-swift-frontend -I %t/ %s -typecheck -verify

// Deserialized computed properties don't have a PatternBindingDecl, so
// make sure we don't expect to find one.

import deserialized_witness_mismatch_other

protocol HasCurrent {
  var current: Self { get }
  // expected-note@-1 {{protocol requires property 'current' with type 'TimeZone'}}
}

extension TimeZone : HasCurrent {}
// expected-error@-1 {{type 'TimeZone' does not conform to protocol 'HasCurrent'}}
// expected-note@-2 {{add stubs for conformance}}
