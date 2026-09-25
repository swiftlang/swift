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

protocol Q: SendableMetatype {
  func g()
}

func acceptSendableP<T: P & Sendable>(_: T) {} // expected-note {{'acceptSendableP' declared here}}

//--- ext.swift
default @MainActor

// File default isolates the extension to @MainActor, creating an isolated
// conformance. Module default would leave extension of nonisolated type
// nonisolated.

/* @MainActor */ extension C: /* @MainActor */ P {
  func f() {}
}

func use(_ c: C) {
  acceptSendableP(c) // expected-error {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

// File default can't create an isolated conformance for SendableMetatype.

extension C: Q {
  // expected-error@-1:14 {{conformance of 'C' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2:14 {{turn data races into runtime errors with '@preconcurrency'}}{{14-14=@preconcurrency }}
  func g() {}
  // expected-note@-1:8 {{main actor-isolated instance method 'g()' cannot satisfy nonisolated requirement}}
  // expected-note@-2:8 {{mark instance method 'g()' 'nonisolated'}}{{3-3=nonisolated }}
}

struct S: Q {
  // expected-error@-1:11 {{conformance of 'S' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2:11 {{turn data races into runtime errors with '@preconcurrency'}}{{11-11=@preconcurrency }}
  func g() {}
  // expected-note@-1:8 {{main actor-isolated instance method 'g()' cannot satisfy nonisolated requirement}}
  // expected-note@-2:8 {{mark instance method 'g()' 'nonisolated'}}{{3-3=nonisolated }}
}
