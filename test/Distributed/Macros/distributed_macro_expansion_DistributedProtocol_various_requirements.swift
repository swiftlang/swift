// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -parse-as-library -I %t %S/../Inputs/FakeDistributedActorSystems.swift -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s --dump-input=always

import Distributed

@_DistributedProtocol
protocol Greeter: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func greet(name: String) -> String
}

// @_DistributedProtocol ->

// CHECK: distributed actor $Greeter: Greeter,
// CHECK:    Distributed._DistributedActorStub
// CHECK: {
// CHECK:   typealias ActorSystem = FakeActorSystem
// CHECK: }

// CHECK: extension Greeter where Self: Distributed._DistributedActorStub {
// CHECK:   distributed func greet(name: String) -> String {
// CHECK:     if #available (SwiftStdlib 6.0, *) {
// CHECK:       Distributed._distributedStubFatalError()
// CHECK:     } else {
// CHECK:       fatalError()
// CHECK:     }
// CHECK:   }
// CHECK: }

@_DistributedProtocol
protocol Greeter2: DistributedActor where ActorSystem: FakeRoundtripActorSystem {
  distributed func greet(name: String) -> String
}

// @_DistributedProtocol ->

// CHECK: distributed actor $Greeter2<ActorSystem>: Greeter2,
// CHECK:   Distributed._DistributedActorStub
// CHECK:   where ActorSystem: FakeRoundtripActorSystem
// CHECK: {
// CHECK: }

// CHECK: extension Greeter2 where Self: Distributed._DistributedActorStub {
// CHECK:   distributed func greet(name: String) -> String {
// CHECK:     if #available (SwiftStdlib 6.0, *) {
// CHECK:       Distributed._distributedStubFatalError()
// CHECK:     } else {
// CHECK:       fatalError()
// CHECK:     }
// CHECK:   }
// CHECK: }
