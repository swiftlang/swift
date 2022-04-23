// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-irgen -module-name distributed_actor_accessors -disable-availability-checking -I %t 2>&1 %s | %IRGenFileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

class MyClass { }

// Ensure that the actor layout is (metadata pointer, default actor, id, system,
// <user fields>)

protocol HasActorSystem {
  var actorSystem: FakeActorSystem { get }
}

extension MyActor: HasActorSystem { }

// CHECK: %T27distributed_actor_accessors7MyActorC = type <{ %swift.refcounted, %swift.defaultactor, %T27FakeDistributedActorSystems0C7AddressV, %T27FakeDistributedActorSystems0aC6SystemV, %T27distributed_actor_accessors7MyClassC* }>
@available(SwiftStdlib 5.7, *)
public distributed actor MyActor {
  var field: MyClass = MyClass()

  init(actorSystem: FakeActorSystem) {
    self.actorSystem = actorSystem
  }
}
