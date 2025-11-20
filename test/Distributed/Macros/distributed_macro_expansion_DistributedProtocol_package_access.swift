// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -package-name myPkg -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s

import Distributed

@Resolvable
package protocol Greeter: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet(name: String) -> String
}

// @Resolvable ->

// CHECK: package distributed actor $Greeter<ActorSystem>: Greeter,
// CHECK-NEXT: Distributed._DistributedActorStub
// CHECK-NEXT: where ActorSystem: DistributedActorSystem<any Codable>
// CHECK-NEXT: {
// CHECK: }

// CHECK: extension Greeter where Self: Distributed._DistributedActorStub {
// CHECK:   package distributed func greet(name: String) -> String {
// CHECK-NEXT:     if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
// CHECK-NEXT:       Distributed._distributedStubFatalError()
// CHECK-NEXT:     } else {
// CHECK-NEXT:       fatalError()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }

