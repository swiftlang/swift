// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls
//
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -plugin-path %swift-plugin-dir -dump-macro-expansions %s 2>&1 | %FileCheck %s

// The stub of a '@Resolvable' protocol whose actor system is constrained to
// be a '_DistributedOnewayActorSystem' keeps every constraint of the actor
// system, so that it conforms to the protocol

@_spi(OnewaySend) import Distributed

@Resolvable
protocol OnlyOneway: DistributedActor where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop()
}

// CHECK:      distributed actor $OnlyOneway<ActorSystem>: OnlyOneway,
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   where ActorSystem: _DistributedOnewayActorSystem<any Codable>
// CHECK-NEXT: {
// CHECK:      }

// CHECK:      extension OnlyOneway where Self: Distributed._DistributedActorStub {
// CHECK-NEXT:   @oneway distributed func stop() {

@Resolvable
protocol Composition: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> & _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop()
}

// CHECK:      distributed actor $Composition<ActorSystem>: Composition,
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   where ActorSystem: DistributedActorSystem<any Codable> & _DistributedOnewayActorSystem<any Codable>
// CHECK-NEXT: {
// CHECK:      }

// Both requirements are kept, in order, not only the last one
@Resolvable
protocol TwoRequirements: DistributedActor where ActorSystem: _DistributedOnewayActorSystem<any Codable>, ActorSystem: DistributedActorSystem<any Codable> {
  @oneway distributed func stop()
}

// CHECK:      distributed actor $TwoRequirements<ActorSystem>: TwoRequirements,
// CHECK-NEXT:   Distributed._DistributedActorStub
// CHECK-NEXT:   where ActorSystem: _DistributedOnewayActorSystem<any Codable>,
// CHECK-NEXT:   ActorSystem: DistributedActorSystem<any Codable>
// CHECK-NEXT: {
// CHECK:      }
