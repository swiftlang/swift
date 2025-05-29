// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -I %t %S/../Inputs/FakeDistributedActorSystems.swift -dump-macro-expansions %s 2>&1

import Distributed

@Resolvable // expected-error{{'@Resolvable' can only be applied to 'protocol', but was attached to 'struct' (from macro 'Resolvable')}}
struct Struct {}

@Resolvable // expected-error{{'@Resolvable' can only be applied to 'protocol', but was attached to 'class' (from macro 'Resolvable')}}
class Clazz {}

@Resolvable // expected-error{{'@Resolvable' can only be applied to 'protocol', but was attached to 'actor' (from macro 'Resolvable')}}
actor Act {}

@Resolvable // expected-error{{'@Resolvable' can only be applied to 'protocol', but was attached to 'actor' (from macro 'Resolvable')}}
distributed actor Caplin {
  typealias ActorSystem = FakeActorSystem
}

@Resolvable // expected-note 4{{in expansion of macro 'Resolvable' on protocol 'Fail' here}}
protocol Fail: DistributedActor {
  distributed func method() -> String
}

@Resolvable // expected-note2{{in expansion of macro 'Resolvable' on protocol 'SomeRoot' here}}
public protocol SomeRoot: DistributedActor, Sendable
  where ActorSystem: DistributedActorSystem<any Codable> {

  associatedtype AssociatedSomething: Sendable // expected-note{{protocol requires nested type 'AssociatedSomething'}}
  static var staticValue: String { get }
  var value: String { get }
}
