// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -target %target-swift-5.1-abi-triple -swift-version 6 -o %t/def_isolated_conformance.swiftmodule %S/Inputs/def_isolated_conformance.swift -default-isolation=MainActor

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 %s -I %t

// REQUIRES: concurrency

import def_isolated_conformance

func acceptMyProtocol(_: some MyProtocol) { }
func acceptOtherProtocol(_: some MyProtocol) { }

nonisolated func f(mc: MyClass) {
  acceptMyProtocol(mc)
  // expected-error@-1{{main actor-isolated conformance of 'MyClass' to 'MyProtocol' cannot be used in nonisolated context}}
  acceptOtherProtocol(mc)
  // expected-error@-1{{main actor-isolated conformance of 'MyClass' to 'MyProtocol' cannot be used in nonisolated context}}
}
