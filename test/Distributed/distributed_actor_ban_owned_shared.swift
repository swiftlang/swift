// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

class Param: Codable {}

distributed actor First {
  distributed func owned(_: __owned Param) async throws {} // expected-error{{cannot declare '__owned' argument '_' in distributed instance method 'owned'}}
  distributed func shared(_: __shared Param) async throws {} // expected-error{{cannot declare '__shared' argument '_' in distributed instance method 'shared'}}
  distributed func consuming(_: consuming Param) async throws {}
  // expected-error@-1{{copyable types cannot be 'consuming' or 'borrowing' yet}}
  // expected-error@-2{{parameter '' of type '<<error type>>' in distributed instance method does not conform to serialization requirement 'Codable'}}
}

func test(first: First) async throws {
  try await first.owned(.init())
  try await first.shared(.init())
}
