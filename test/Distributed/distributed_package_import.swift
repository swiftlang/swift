// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -package-name MyModule -module-name MyModule -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

// Should NOT produce a warning, we do use the module in a public declaration, see MyPublicDistributedActor
package import Distributed
import FakeDistributedActorSystems


package distributed actor MyPublicDistributedActor {
  package typealias ActorSystem = FakeActorSystem
}
