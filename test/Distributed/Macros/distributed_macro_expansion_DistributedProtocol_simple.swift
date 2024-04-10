// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s

import Distributed

@_DistributedProtocol
protocol Greeter: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet(name: String) -> String
}

// @_DistributedProtocol ->

// CHECK: distributed actor $Greeter<ActorSystem>: Greeter,
// CHECK-NEXT: Distributed._DistributedActorStub
// CHECK-NEXT: where ActorSystem: DistributedActorSystem<any Codable>
// CHECK-NEXT: {
// CHECK-NEXT: }

// CHECK: extension Greeter where Self: Distributed._DistributedActorStub {
// CHECK-NEXT:   distributed func greet(name: String) -> String {
// CHECK-NEXT:     if #available(SwiftStdlib 6.0, *) {
// CHECK-NEXT:       Distributed._distributedStubFatalError()
// CHECK-NEXT:     } else {
// CHECK-NEXT:       fatalError()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }
