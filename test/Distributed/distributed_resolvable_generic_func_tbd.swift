// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/Inputs/FakeDistributedActorSystems.swift

// RUN: %target-swift-frontend -emit-ir -validate-tbd-against-ir=all -enable-library-evolution -target %target-swift-6.0-abi-triple -parse-as-library -plugin-path %swift-plugin-dir -module-name Library -I %t %s -o /dev/null

// REQUIRES: swift_swift_parser, asserts
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// A generic requirement whose type parameter has a single protocol bound works
@Resolvable
public protocol SingleBound: DistributedActor where Self.ActorSystem == FakeActorSystem {
  distributed func single<T: Codable>(_ gen: T) -> T
}

// A generic requirement whose type parameter has two protocol bounds
@Resolvable
public protocol TwoBounds: DistributedActor where Self.ActorSystem == FakeActorSystem {
  distributed func poly<T: Sendable & Codable>(_ gen: T) -> T
}
