// RUN: %target-swift-frontend -c -verify -primary-file %s %S/Inputs/other_file_protocol_default_implementation_witness.swift

// https://github.com/apple/swift/issues/55897
// Test missing protocol requirement `@differentiable` attribute errors for
// protocol witnesses declared in a different file than the protocol
// conformance.
//
// This test case specifically tests protocol extension method witnesses.

import _Differentiation

// expected-error @+2 {{type 'ConformingStruct' does not conform to protocol 'P1'}} 
// expected-note @+1 {{add stubs for conformance}}
struct ConformingStruct: P2 {}
