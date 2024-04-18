// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -parse-as-library -I %t %S/../Inputs/FakeDistributedActorSystems.swift -dump-macro-expansions %s 2>&1

import Distributed

@_DistributedProtocol // expected-error{{'@DistributedProtocol' can only be applied to 'protocol', but was attached to 'struct' (from macro '_DistributedProtocol')}}
struct Struct {}

@_DistributedProtocol // expected-error{{'@DistributedProtocol' can only be applied to 'protocol', but was attached to 'class' (from macro '_DistributedProtocol')}}
class Clazz {}

@_DistributedProtocol // expected-error{{'@DistributedProtocol' can only be applied to 'protocol', but was attached to 'actor' (from macro '_DistributedProtocol')}}
actor Act {}

@_DistributedProtocol // expected-error{{'@DistributedProtocol' can only be applied to 'protocol', but was attached to 'actor' (from macro '_DistributedProtocol')}}
distributed actor Caplin {
  typealias ActorSystem = FakeActorSystem
}

@_DistributedProtocol // expected-error{{Distributed protocol must declare actor system with SerializationRequirement}}
protocol Fail: DistributedActor {
  distributed func method() -> String
}

@_DistributedProtocol // expected-note{{in expansion of macro '_DistributedProtocol' on protocol 'SomeRoot' here}}
public protocol SomeRoot: DistributedActor, Sendable
  where ActorSystem: DistributedActorSystem<any Codable> {

  // TODO(distributed): we could diagnose this better?
  associatedtype AssociatedSomething: Sendable // expected-note{{protocol requires nested type 'AssociatedSomething'; add nested type 'AssociatedSomething' for conformance}}
  static var staticValue: String { get }
  var value: String { get }
}
