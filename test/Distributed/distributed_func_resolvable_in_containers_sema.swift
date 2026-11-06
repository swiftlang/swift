// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix enabled- -verify-ignore-unrelated -verify-ignore-unknown -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature DistributedAnyResolvableInContainers 2>&1 %s
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix disabled- -verify-ignore-unrelated -verify-ignore-unknown -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -I %t 2>&1 %s
// REQUIRES: swift_swift_parser, asserts
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_DistributedAnyResolvableInContainers

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func greet() -> String
}

@Resolvable
protocol PowerSwitch: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func toggle()
}

struct NotCodable: Hashable {}

// ==== -----------------------------------------------------------------------
// MARK: Containers of a @Resolvable existential

distributed actor Worker {
  typealias ActorSystem = FakeActorSystem

  // expected-disabled-error@+1{{parameter '_' of type '[any Greeter]' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func takeArray(_ gs: [any Greeter]) {}
  // expected-disabled-error@+1{{parameter '_' of type '(any Greeter)?' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func takeOptional(_ g: (any Greeter)?) {}
  // expected-disabled-error@+1{{parameter '_' of type '[String : any Greeter]' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func takeDictionary(_ gs: [String: any Greeter]) {}

  // expected-disabled-error@+1{{result type '[any Greeter]' of distributed instance method 'returnArray' does not conform to serialization requirement 'Codable'}}
  distributed func returnArray() -> [any Greeter] { [] }
  // expected-disabled-error@+1{{result type '(any Greeter)?' of distributed instance method 'returnOptional' does not conform to serialization requirement 'Codable'}}
  distributed func returnOptional() -> (any Greeter)? { nil }
  // expected-disabled-error@+1{{result type '[String : any Greeter]' of distributed instance method 'returnDictionary' does not conform to serialization requirement 'Codable'}}
  distributed func returnDictionary() -> [String: any Greeter] { [:] }

  // expected-disabled-error@+1{{result type '[any Greeter]' of distributed property 'arrayProperty' does not conform to serialization requirement 'Codable'}}
  distributed var arrayProperty: [any Greeter] { [] }

  // OK: `& Hashable` is redundant, a distributed actor protocol is Hashable
  distributed func returnHashable() -> any (Greeter & Hashable) { fatalError() }
}

// ==== -----------------------------------------------------------------------
// MARK: The wire type must conform to the serialization requirement

distributed actor BadWireTypes {
  typealias ActorSystem = FakeActorSystem

  // expected-error@+1{{result type '[NotCodable : any Greeter]' of distributed instance method 'nonCodableKey' does not conform to serialization requirement 'Codable'}}
  distributed func nonCodableKey() -> [NotCodable: any Greeter] { [:] }

  // InlineArray is not Codable
  // expected-error@+2{{parameter '_' of type 'InlineArray<2, any Greeter>' in distributed instance method does not conform to serialization requirement 'Codable'}}
  @available(SwiftStdlib 6.2, *)
  distributed func inlineArray(_ gs: InlineArray<2, any Greeter>) {}
}

// ==== -----------------------------------------------------------------------
// MARK: Shapes which aren't supported keep today's errors

distributed actor UnsupportedShapes {
  typealias ActorSystem = FakeActorSystem

  // expected-error@+1{{parameter '_' of type '[[any Greeter]]' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func nested(_ gs: [[any Greeter]]) {}

  // expected-error@+1{{parameter '_' of type '[some Greeter]' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func someElements(_ gs: [some Greeter]) {}

  // expected-error@+3{{type 'any Greeter' cannot conform to 'Hashable'}}
  // expected-note@+2{{only concrete types such as structs, enums and classes can conform to protocols}}
  // expected-error@+1{{parameter '_' of type '_' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func set(_ gs: Set<any Greeter>) {}
}

// ==== -----------------------------------------------------------------------
// MARK: Element checks apply inside containers

distributed actor BadElements {
  typealias ActorSystem = FakeActorSystem

  // expected-enabled-error@+3{{parameter '_' of type '[any Greeter & PowerSwitch]' in distributed instance method contains more than one '@Resolvable protocol'}}
  // expected-enabled-note@+2{{declare a new '@Resolvable protocol' that refines those protocols and use it instead}}
  // expected-disabled-error@+1{{parameter '_' of type '[any Greeter & PowerSwitch]' in distributed instance method does not conform to serialization requirement 'Codable'}}
  distributed func ambiguous(_ gs: [any Greeter & PowerSwitch]) {}
}
