// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls
// REQUIRES: OS=macosx
//
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -enable-experimental-feature SendCalls -plugin-path %swift-plugin-dir -parse-as-library -I %t %s -dump-macro-expansions 2>&1 | %FileCheck %s

// The '@Resolvable' stubs keep the '@oneway' attribute of the requirements

import Distributed
import FakeDistributedActorSystems

@Resolvable
@available(SwiftStdlib 6.5, *)
protocol Mixed: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func plain() -> String
  @oneway distributed func notify(_ value: Int)
  @oneway distributed func notifyAsync() async
}

// CHECK: extension Mixed where Self: Distributed._DistributedActorStub
// CHECK-NEXT: distributed func plain() -> String {
// CHECK-NOT:  @oneway
// CHECK:      @oneway distributed func notify(_ value: Int) {
// CHECK:        Distributed._distributedStubFatalError()
// CHECK:      @oneway distributed func notifyAsync() async {
// CHECK:        Distributed._distributedStubFatalError()
