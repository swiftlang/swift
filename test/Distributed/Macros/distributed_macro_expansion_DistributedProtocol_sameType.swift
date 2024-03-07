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

// CHECK:      distributed actor $Greeter: Greeter,
// CHECK-NEXT:    Distributed._DistributedActorStub
// CHECK-NEXT: {
// CHECK-NEXT:   typealias ActorSystem = FakeActorSystem
// CHECK-NEXT: }

// CHECK: extension Greeter where Self: Distributed._DistributedActorStub {
// CHECK-NEXT:   distributed func greet(name: String) -> String {
// CHECK-NEXT:     if #available (SwiftStdlib 6.0, *) {
// CHECK-NEXT:       Distributed._distributedStubFatalError()
// CHECK-NEXT:     } else {
// CHECK-NEXT:       fatalError()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }
