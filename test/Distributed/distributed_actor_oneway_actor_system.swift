// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -plugin-path %swift-plugin-dir -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SendCalls

// The synchronous distributed thunk of a '@oneway' function hands a remote call
// to the 'remoteCallOneway' of a '_DistributedOnewayActorSystem', so a '@oneway'
// distributed function, or requirement, can only be declared where the actor
// system is statically known to conform to it: a concrete actor system which
// conforms, or a generic one which is constrained to conform

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

// FakeActorSystem is a '_DistributedOnewayActorSystem', FakeActorSystemWithSPI
// is not
typealias TwoWaySystem = FakeActorSystemWithSPI

// ==== -----------------------------------------------------------------------
// MARK: Concrete actor systems

distributed actor Concrete { // expected-note{{actor system 'Concrete.ActorSystem' (aka 'FakeActorSystemWithSPI') of distributed actor 'Concrete' does not conform to '_DistributedOnewayActorSystem'}}
  typealias ActorSystem = TwoWaySystem

  @oneway distributed func stop() {}
  // expected-error@-1{{'@oneway' distributed instance method 'stop()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}

  // A two-way function is fine
  distributed func twoWay() {}
}

distributed actor ConcreteGood {
  typealias ActorSystem = FakeActorSystem

  @oneway distributed func stop() {}
  @oneway distributed func later() async {}
}

func sendConcrete(_ a: ConcreteGood) throws {
  try send a.stop()
  try send a.later()
}

// ==== -----------------------------------------------------------------------
// MARK: Generic actor systems

distributed actor Generic<ActorSystem> where ActorSystem: DistributedActorSystem<any Codable> { // expected-note{{actor system 'ActorSystem' of generic distributed actor 'Generic' does not conform to '_DistributedOnewayActorSystem'}}
  @oneway distributed func stop() {}
  // expected-error@-1{{'@oneway' distributed instance method 'stop()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}
}

distributed actor GenericGood<ActorSystem> where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop() {}
}

// The constraint may also come from the extension
distributed actor GenericExtension<ActorSystem> where ActorSystem: DistributedActorSystem<any Codable> {}

extension GenericExtension where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func stop() {}
}

func sendGeneric<S: _DistributedOnewayActorSystem<any Codable>>(
  _ a: GenericGood<S>, _ b: GenericExtension<S>
) throws {
  try send a.stop()
  try send b.stop()
}

func sendGenericConcrete(_ a: GenericGood<FakeRoundtripActorSystem>) throws {
  try send a.stop()
}

// ==== -----------------------------------------------------------------------
// MARK: Distributed protocols

protocol Unconstrained: DistributedActor { // expected-note 2{{actor system 'Self.ActorSystem' of protocol 'Unconstrained' does not conform to '_DistributedOnewayActorSystem'}}
  @oneway distributed func stop()
  // expected-error@-1{{'@oneway' distributed instance method 'stop()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}
}

extension Unconstrained {
  @oneway distributed func stopLater() {}
  // expected-error@-1{{'@oneway' distributed instance method 'stopLater()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}
}

protocol ConstrainedTwoWay: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> { // expected-note{{actor system 'Self.ActorSystem' of protocol 'ConstrainedTwoWay' does not conform to '_DistributedOnewayActorSystem'}}
  @oneway distributed func stop()
  // expected-error@-1{{'@oneway' distributed instance method 'stop()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}
}

protocol ConcreteTwoWay: DistributedActor where ActorSystem == TwoWaySystem { // expected-note{{actor system 'FakeActorSystemWithSPI' of protocol 'ConcreteTwoWay' does not conform to '_DistributedOnewayActorSystem'}}
  @oneway distributed func stop()
  // expected-error@-1{{'@oneway' distributed instance method 'stop()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}
}

protocol ConstrainedOneway: DistributedActor where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop()
}

protocol ConcreteOneway: DistributedActor where ActorSystem == FakeActorSystem {
  @oneway distributed func stop()
}

func sendProtocols(_ a: some ConstrainedOneway, _ b: any ConcreteOneway) throws {
  try send a.stop()
  try send b.stop()
}

// ==== -----------------------------------------------------------------------
// MARK: '@Resolvable' protocols

// Diagnosed once, at the requirement, not again at the stub of the macro
@Resolvable
protocol ResolvableTwoWay: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> { // expected-note{{actor system 'Self.ActorSystem' of protocol 'ResolvableTwoWay' does not conform to '_DistributedOnewayActorSystem'}}
  @oneway distributed func stop()
  // expected-error@-1{{'@oneway' distributed instance method 'stop()' requires its actor system to conform to '_DistributedOnewayActorSystem'}}
}

// The 'SerializationRequirement' is the primary associated type of
// '_DistributedOnewayActorSystem' as well
@Resolvable
@_spi(OnewaySend)
public protocol MyService: DistributedActor where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop()
  distributed func ping() -> String
}

@Resolvable
@_spi(OnewaySend)
public protocol MyServiceComposition: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> & _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop()
}

@Resolvable
@_spi(OnewaySend)
public protocol MyServiceTwoRequirements: DistributedActor where ActorSystem: _DistributedOnewayActorSystem<any Codable>, ActorSystem: DistributedActorSystem<any Codable> {
  @oneway distributed func stop()
}

@Resolvable
protocol DistributedWorker: DistributedActor where ActorSystem == FakeActorSystem {
  @oneway distributed func stop()
}

distributed actor Service: MyService, MyServiceComposition, MyServiceTwoRequirements, DistributedWorker {
  typealias ActorSystem = FakeActorSystem

  @oneway distributed func stop() {}
  distributed func ping() -> String { "pong" }
}

func sendStubs(_ system: FakeActorSystem, _ id: ActorAddress) throws {
  try send $MyService<FakeActorSystem>.resolve(id: id, using: system).stop()
  try send $MyServiceComposition<FakeActorSystem>.resolve(id: id, using: system).stop()
  try send $MyServiceTwoRequirements<FakeActorSystem>.resolve(id: id, using: system).stop()
  try send $DistributedWorker.resolve(id: id, using: system).stop()
}
