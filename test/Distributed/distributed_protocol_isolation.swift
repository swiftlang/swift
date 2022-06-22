// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -strict-concurrency=targeted -disable-availability-checking -I %t 2>&1 %s
// XXX: %target-swift-frontend -Xllvm -swift-diagnostics-assert-on-warning=1 -Xllvm -swift-diagnostics-assert-on-error=1 -typecheck -verify -strict-concurrency=targeted -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

/// Use the existential wrapper as the default actor system.
typealias DefaultDistributedActorSystem = FakeActorSystem

protocol TerminationWatchingDA: DistributedActor {
  func terminated(da: String) async
}

distributed actor DA_TerminationWatchingDA: TerminationWatchingDA {
  distributed func test() {}
  func terminated(da: String) { }
}

func test_watchingDA<WDA: TerminationWatchingDA>(da: WDA) async throws {
  await da.whenLocal { localDA in
    await localDA.terminated(da: "local calls are okey!") // OK
  }
}
