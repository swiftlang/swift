// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature DistributedRemoteBlockingCalls -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_DistributedRemoteBlockingCalls

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ------------------------------------------------------------------------
// MARK: Positive cases: @remoteCall(blocking) on distributed members

distributed actor Greeter {
  // An async '@remoteCall(blocking)' method is accepted.
  @remoteCall(blocking)
  distributed func hello(name: String) async -> Int { 0 }

  @remoteCall(blocking)
  distributed func ping() async {}

  // A blocking computed property is accepted.
  @remoteCall(blocking)
  distributed var status: Int { 0 }
}

func callGreeter(_ g: Greeter) async throws {
  let _: Int = try await g.hello(name: "a")
  _ = try await g.status
}

// ==== ------------------------------------------------------------------------
// MARK: Negative cases: @remoteCall(blocking) requires a 'distributed' member

struct NotAnActor {
  // expected-error@+1{{'@remoteCall' can only be applied to 'distributed' methods and computed properties}}
  @remoteCall(blocking)
  func plain() {}
}

distributed actor MixedGreeter {
  // '@remoteCall(blocking)' on a non-'distributed' method is rejected.
  // expected-error@+1{{'@remoteCall' can only be applied to 'distributed' methods and computed properties}}
  @remoteCall(blocking)
  func localOnly() {}

  // '@remoteCall(blocking)' on a non-'distributed' computed property is rejected.
  // expected-error@+1{{'@remoteCall' can only be applied to 'distributed' methods and computed properties}}
  @remoteCall(blocking)
  var localOnlyStatus: Int { 0 }
}
