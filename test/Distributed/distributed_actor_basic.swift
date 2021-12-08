// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA {
}

distributed actor First {
  distributed func one(second: Second) async throws {
    try await second.two(first: self, second: second)
  }
}

distributed actor Second {
  distributed func two(first: First, second: Second) async {
    try! await first.one(second: self)
  }
}

// ==== ------------------------------------------------------------------------

extension First {
  @_dynamicReplacement (for :_remote_one(second:))
  nonisolated func _impl_one(second: Second) async throws {
    fatalError()
  }
}

extension Second {
  @_dynamicReplacement (for :_remote_two(first:second:))
  nonisolated func _impl_two(first: First, second: Second) async throws {
    fatalError()
  }
}
