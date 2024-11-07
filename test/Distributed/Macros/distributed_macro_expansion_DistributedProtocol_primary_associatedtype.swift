// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s 2>&1 | %FileCheck %s

import Distributed

typealias System = LocalTestingDistributedActorSystem

@Resolvable
protocol Base<Fruit>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  associatedtype Fruit: Codable
  distributed func get() -> Fruit
}
// CHECK: distributed actor $Base<ActorSystem, Fruit>: Base
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   where ActorSystem: DistributedActorSystem<any Codable>,
// CHECK-NEXT:   Fruit: Codable
// CHECK-NEXT: {
// CHECK: }

@Resolvable
protocol Base2<Fruit, Animal>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  associatedtype Fruit: Codable
  associatedtype Animal: Codable
  distributed func get(animal: Animal) -> Fruit
}
// CHECK: distributed actor $Base2<ActorSystem, Fruit, Animal>: Base
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   where ActorSystem: DistributedActorSystem<any Codable>,
// CHECK-NEXT:   Fruit: Codable,
// CHECK-NEXT:   Animal: Codable
// CHECK-NEXT: {
// CHECK: }

@Resolvable
protocol Base3<Fruit, Animal>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  associatedtype Fruit: Codable
  associatedtype Animal: Codable & Hashable
  distributed func get(animal: Animal) -> Fruit
}
// CHECK: distributed actor $Base3<ActorSystem, Fruit, Animal>: Base
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   where ActorSystem: DistributedActorSystem<any Codable>,
// CHECK-NEXT:   Fruit: Codable,
// CHECK-NEXT:   Animal: Codable & Hashable
// CHECK-NEXT: {
// CHECK: }

/// This type is not generic over the actor system because of the == constraint:
@Resolvable
protocol Base4<Fruit, Animal>: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem{
  associatedtype Fruit: Codable
  associatedtype Animal: Codable & Hashable

  distributed func get(animal: Animal) -> Fruit
}
// CHECK: distributed actor $Base4<Fruit, Animal>: Base
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   Fruit: Codable,
// CHECK-NEXT:   Animal: Codable & Hashable
// CHECK-NEXT: {
// CHECK: }
