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

protocol Greeter: DistributedActor {
  distributed func get() -> String
  distributed func greet(name: String) -> String
}

// ==== ------------------------------------------------------------------------

@freestanding(declaration, names: named(get()), named(greet(name:)))
macro _distributed_stubs_Greeter() =
  #distributedStubs( // in distributed macros
    module: "main", protocolName: "Greeter",
    stubProtocols: [],
    "distributed func get() -> String",
    "distributed func greet(name: String) -> String"
  )

// ==== ------------------------------------------------------------------------

distributed actor StubTest_expression: Greeter {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  #_distributed_stubs_Greeter
  // CHECK:   distributed func get() -> String {
  // CHECK:     Swift.fatalError()
  // CHECK:   }
  // CHECK:   distributed func greet(name: String) -> String {
  // CHECK:     Swift.fatalError()
  // CHECK:   }
}

