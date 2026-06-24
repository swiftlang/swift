// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -verify %t/protocols.swift %t/conformances.swift
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -enable-experimental-feature NoExplicitNonIsolated -typecheck -swift-version 6 -verify -verify-additional-prefix no-explicit-nonisolated- %t/protocols.swift %t/conformances.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile
// REQUIRES: swift_feature_NoExplicitNonIsolated

//--- protocols.swift

// unspecified
protocol PlainProto {
  func plain()
}

// explicit nonisolated
nonisolated
protocol NonIsoProto {
  func nonisolated()
}

// @MainActor
@MainActor
protocol IsoProto {
  func isolated()
}

// unspecified, and SendableMetatype would normally ask conformances to be nonisolated
protocol SendableMetatypeProto: SendableMetatype {
  func sm()
}

//--- conformances.swift

using @MainActor

class A {}
extension A: PlainProto {
  // expected-error@-1 {{conformance of 'A' to protocol 'PlainProto' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
  // expected-note@-3 {{isolate this conformance to the main actor with '@MainActor'}}
  func plain() {}
  // expected-note@-1 {{main actor-isolated instance method 'plain()' cannot satisfy nonisolated requirement}}
  // expected-note@-2 {{mark instance method 'plain()' 'nonisolated'}}
}

class B {}
extension B: NonIsoProto {
  // expected-no-explicit-nonisolated-error@-1 {{conformance of 'B' to protocol 'NonIsoProto' crosses into main actor-isolated code and can cause data races}}
  // expected-no-explicit-nonisolated-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
  // expected-no-explicit-nonisolated-note@-3 {{isolate this conformance to the main actor with '@MainActor'}}
  func nonisolated() {}
  // expected-no-explicit-nonisolated-note@-1 {{main actor-isolated instance method 'nonisolated()' cannot satisfy nonisolated requirement}}
  // expected-no-explicit-nonisolated-note@-2 {{mark instance method 'nonisolated()' 'nonisolated'}}
}

class C {}
extension C: IsoProto {
  func isolated() {}
}

class D {}
extension D: SendableMetatypeProto {
  // expected-error@-1 {{conformance of 'D' to protocol 'SendableMetatypeProto' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
  // expected-note@-3 {{isolate this conformance to the main actor with '@MainActor'}}
  func sm() {}
  // expected-note@-1 {{main actor-isolated instance method 'sm()' cannot satisfy nonisolated requirement}}
  // expected-note@-2 {{mark instance method 'sm()' 'nonisolated'}}
}
