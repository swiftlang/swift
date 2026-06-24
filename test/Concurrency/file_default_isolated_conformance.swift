// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -enable-experimental-feature DefaultIsolationPerFile -enable-upcoming-feature InferIsolatedConformances %t/types.swift %t/ext.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile
// REQUIRES: swift_feature_InferIsolatedConformances

//--- types.swift
struct C {}

protocol P {
  func f()
}

func acceptSendableP<T: P & Sendable>(_: T) {} // expected-note {{'acceptSendableP' declared here}}

//--- ext.swift
using @MainActor

// The file default isolates the extension to the main actor with no SE-0466
// carve-out, so the inferred conformance is main-actor-isolated.
extension C: P {
  func f() {}
}

func use(_ c: C) {
  acceptSendableP(c) // expected-error {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}
