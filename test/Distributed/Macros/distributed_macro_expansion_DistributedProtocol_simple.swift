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

@DistributedProtocol // TODO: attach automatically
protocol G1: DistributedActor where SerializationRequirement == any Codable {
  distributed func get() -> String
  distributed func greet(name: String) -> String
}

// @DistributedProtocol ->
//
// CHECK: @freestanding(declaration, names: named(get()), named(greet(name:)))
// CHECK: macro _distributed_stubs_G1() =
// CHECK:   #distributedStubs(
// CHECK:     module: "main", protocolName: "G1",
// CHECK:     stubProtocols: [],
// CHECK:     "distributed func get() -> String",
// CHECK:     "distributed func greet(name: String) -> String"
// CHECK:   )
//
// TODO: distributed actor $G1<ActorSystem>: Greeter where SerializationRequirement == any Codable {
// CHECK: distributed actor $G1: G1 {
// TODO:    Preferably, we could refer to our own macro like this: #_distributed_stubs_G2
// WORKAROUND:
// CHECK:   #distributedStubs(
// CHECK:      module: "main", protocolName: "G1",
// CHECK:      stubProtocols: [],
// CHECK:      "distributed func get() -> String",
// CHECK:      "distributed func greet(name: String) -> String"
// CHECK:    )
// CHECK: }

// ==== ------------------------------------------------------------------------

@DistributedProtocol // TODO: attach automatically
protocol G2: DistributedActor where SerializationRequirement == any Codable {
  distributed func get() -> String
  distributed func greet(name: String) -> String

  func local(check: Int) -> Int
}

// @DistributedProtocol ->
//
// CHECK: @freestanding(declaration, names: named(get()), named(greet(name:)), named(local(check:)))
// CHECK: macro _distributed_stubs_G2() =
// CHECK:   #distributedStubs(
// CHECK:     module: "main", protocolName: "G2",
// CHECK:     stubProtocols: [],
// CHECK:     "distributed func get() -> String",
// CHECK:     "distributed func greet(name: String) -> String"
// CHECK:   )
//
// TODO: distributed actor $G2<ActorSystem>: Greeter where SerializationRequirement == any Codable {
// CHECK: distributed actor $G2: G2 {
// TODO:    Preferably, we could refer to our own macro like this: #_distributed_stubs_G2
// WORKAROUND:
// CHECK:   #distributedStubs(
// CHECK:      module: "main", protocolName: "G2",
// CHECK:      stubProtocols: [],
// CHECK:      "distributed func get() -> String",
// CHECK:      "distributed func greet(name: String) -> String",
// CHECK:      "func local(check: Int) -> Int"
// CHECK:    )
// CHECK: }

// ==== ------------------------------------------------------------------------
