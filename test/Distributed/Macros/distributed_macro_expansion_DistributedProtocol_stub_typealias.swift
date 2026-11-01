// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -enable-experimental-feature DistributedProtocolStubTypealias -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s 2>&1 | %FileCheck %s

import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

@Resolvable
protocol Pinned: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed func ping() -> String
}
// CHECK:      extension Pinned where Self.ActorSystem == LocalTestingDistributedActorSystem {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $Pinned
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

@Resolvable
protocol Greeter: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet(name: String) -> String
}
// CHECK:      extension Greeter where Self.ActorSystem: DistributedActorSystem<any Codable> {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $Greeter<Self.ActorSystem>
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

@Resolvable
protocol WithPrimary<Item>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  associatedtype Item: Codable
  distributed func get() -> Item
}
// CHECK:      extension WithPrimary where Self.ActorSystem: DistributedActorSystem<any Codable> {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $WithPrimary<Self.ActorSystem, Self.Item>
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

// ==== -----------------------------------------------------------------------
// MARK: A base protocol that requires a stub type

// A base protocol may make 'DistributedProtocolStub' a hard requirement. Every
// conforming actor must then resolve it to a single '_DistributedActorStub'.
protocol Servable: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  associatedtype DistributedProtocolStub: Distributed._DistributedActorStub
}

@Resolvable
protocol Inherited: Servable {
  distributed func pong() -> String
}
// CHECK:      extension Inherited where Self.ActorSystem: DistributedActorSystem {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $Inherited
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

// ==== -----------------------------------------------------------------------
// MARK: Refining a @Resolvable protocol with '_emitStubTypealias: false'

// 'Refined' refines 'Servable' and synthesizes 'DistributedProtocolStub = $Refined'
@Resolvable
protocol Refined: Servable {
  distributed func p() -> String
}
// CHECK:      extension Refined where Self.ActorSystem: DistributedActorSystem {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $Refined
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

// A plain '@Resolvable' here would make the generated '$RefinedMore' inherit two
// 'DistributedProtocolStub' witnesses ($Refined via 'Refined' and $RefinedMore
// via its own synthesized typealias), so neither '$RefinedMore' nor an actor
// conforming to 'RefinedMore' would satisfy 'Servable'. Opting out of the
// synthesis with '_emitStubTypealias: false' keeps the single inherited witness
@Resolvable(_emitStubTypealias: false)
protocol RefinedMore: Refined {
  distributed func q() -> String
}

// Compiles: 'DistributedProtocolStub' resolves to the inherited '$Refined'
distributed actor RefinedActor: RefinedMore {
  distributed func p() -> String { "p" }
  distributed func q() -> String { "q" }
}

// ==== -----------------------------------------------------------------------
// MARK: Two independent @Resolvable protocols

@Resolvable
protocol IndepA: Servable {
  distributed func a() -> String
}
// CHECK:      extension IndepA where Self.ActorSystem: DistributedActorSystem {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $IndepA
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

@Resolvable
protocol IndepB: Servable {
  distributed func b() -> String
}
// CHECK:      extension IndepB where Self.ActorSystem: DistributedActorSystem {
// CHECK-NEXT:   #if $DistributedProtocolStubTypealias
// CHECK-NEXT:   typealias DistributedProtocolStub = $IndepB
// CHECK-NEXT:   #endif
// CHECK-NEXT: }

// An actor conforming to both inherits two 'DistributedProtocolStub' witnesses
// ($IndepA and $IndepB), so it must pick one explicitly to satisfy 'Servable'.
distributed actor CombinedPick: IndepA, IndepB {
  typealias DistributedProtocolStub = $IndepB
  distributed func a() -> String { "a" }
  distributed func b() -> String { "b" }
}
