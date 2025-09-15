// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -typecheck -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s --color

import Distributed

@Resolvable
public protocol Greeter: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  @available(iOS 6666, macOS 7777, tvOS 8888, visionOS 9999, *)
  distributed func greet(name: String) -> String

  @available(iOS 6666, macOS 7777, tvOS 8888, visionOS 9999, *)
  distributed var name: String { get }
}

// @Resolvable ->

// CHECK: public distributed actor $Greeter<ActorSystem>: Greeter,
// CHECK:   Distributed._DistributedActorStub
// CHECK:   where ActorSystem: DistributedActorSystem<any Codable>
// CHECK: {
// CHECK: }

// CHECK: extension Greeter where Self: Distributed._DistributedActorStub {
// CHECK:   @available(iOS 6666, macOS 7777, tvOS 8888, visionOS 9999, *)
// CHECK:   public
// CHECK:   distributed func greet(name: String) -> String {
// CHECK:     if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
// CHECK:       Distributed._distributedStubFatalError()
// CHECK:     } else {
// CHECK:       fatalError()
// CHECK:     }
// CHECK:   }
   
// CHECK:   @available(iOS 6666, macOS 7777, tvOS 8888, visionOS 9999, *)
// CHECK:   public
// CHECK:   distributed var name : String {
// CHECK:     get  {
// CHECK:       if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
// CHECK:         Distributed._distributedStubFatalError()
// CHECK:       } else {
// CHECK:         fatalError()
// CHECK:       }
// CHECK:     }
// CHECK:   }
   
// CHECK: }
