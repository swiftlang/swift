// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -I %t -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s --dump-input=always

import Distributed
import FakeDistributedActorSystems

@Resolvable
@_spi(CoolFeatures)
public protocol Greeter: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet(name: String) -> String
}

// @Resolvable ->

// CHECK: @_spi(CoolFeatures)
// CHECK: distributed actor $Greeter<ActorSystem>: Greeter,
// CHECK-NEXT: Distributed._DistributedActorStub
// CHECK-NEXT: where ActorSystem: DistributedActorSystem<any Codable>
// CHECK-NEXT: {
// CHECK: }

// CHECK: @_spi(CoolFeatures)
// CHECK: extension Greeter where Self: Distributed._DistributedActorStub {
// CHECK:       distributed func greet(name: String) -> String {
// CHECK-NEXT:     if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
// CHECK-NEXT:       Distributed._distributedStubFatalError()
// CHECK-NEXT:     } else {
// CHECK-NEXT:       fatalError()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }

@_spi(DistributedSPIForTesting)
@Resolvable
public protocol GreeterWithSPISystem: DistributedActor where ActorSystem == FakeActorSystemWithSPI {
  distributed func greet(name: String) -> String
}

// @Resolvable ->

// CHECK: @_spi(DistributedSPIForTesting)
// CHECK: public distributed actor $GreeterWithSPISystem: GreeterWithSPISystem,
// CHECK-NEXT: Distributed._DistributedActorStub
// CHECK-NEXT: {
// CHECK:   public typealias ActorSystem = FakeActorSystemWithSPI
// CHECK-NEXT: }

// CHECK: @_spi(DistributedSPIForTesting)
// CHECK: extension GreeterWithSPISystem where Self: Distributed._DistributedActorStub {
// CHECK:       public distributed func greet(name: String) -> String {
// CHECK-NEXT:     if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
// CHECK-NEXT:       Distributed._distributedStubFatalError()
// CHECK-NEXT:     } else {
// CHECK-NEXT:       fatalError()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }
