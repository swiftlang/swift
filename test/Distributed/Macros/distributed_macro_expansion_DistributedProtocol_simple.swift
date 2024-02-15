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

// FIXME: the errors below are bugs: the added methods should be considered witnesses rdar://123012943
// expected-note@+1{{in expansion of macro '_DistributedProtocol' on protocol 'Greeter' here}}
@_DistributedProtocol
protocol Greeter: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  // FIXME: this is a bug
  // expected-note@+1{{protocol requires function 'greet(name:)' with type '(String) -> String'}}
  distributed func greet(name: String) -> String
}

// @_DistributedProtocol ->

// CHECK: distributed actor $Greeter<ActorSystem>: Greeter, _DistributedActorStub
// CHECK:   where ActorSystem: DistributedActorSystem<any Codable>,
// CHECK:   ActorSystem.ActorID: Codable
// CHECK: {
// CHECK: }

// CHECK: extension Greeter {
// CHECK:   distributed func greet(name: String) -> String {
// CHECK:     if #available (SwiftStdlib 5.11, *) {
// CHECK:       Distributed._distributedStubFatalError()
// CHECK:     } else {
// CHECK:       fatalError()
// CHECK:     }
// CHECK:   }
// CHECK: }