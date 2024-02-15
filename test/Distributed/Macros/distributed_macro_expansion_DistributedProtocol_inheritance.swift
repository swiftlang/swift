// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s

// FIXME: inheritance tests limited because cannot refer to any generated macro from the same module...
// XFAIL: *

import Distributed

typealias System = LocalTestingDistributedActorSystem

@_DistributedProtocol
protocol EmptyBase {}

// TODO: allow this?
//@_DistributedProtocol
//extension EmptyBase {}

// @_DistributedProtocol ->
//
// CHECK: @freestanding(declaration)
// CHECK: macro _distributed_stubs_EmptyBase() =
// CHECK:   #distributedStubs(
// CHECK:     module: "main", protocolName: "EmptyBase",
// CHECK:     stubProtocols: []
// CHECK:   )
//
// CHECK: // distributed actor $EmptyBase <ActorSystem>: EmptyBase  where SerializationRequirement == any Codable {
// CHECK: distributed actor $EmptyBase : EmptyBase  {
// CHECK:   typealias ActorSystem = LocalTestingDistributedActorSystem // FIXME: remove this
//
// CHECK:   #distributedStubs(
// CHECK:     module: "main", protocolName: "EmptyBase",
// CHECK:     stubProtocols: []
// CHECK:   )
// CHECK: }

@_DistributedProtocol
protocol G3: DistributedActor, EmptyBase where SerializationRequirement == any Codable {
  distributed func get() -> String
  distributed func greet(name: String) -> String
}

// @_DistributedProtocol ->
//
// Since we have also the EmptyBase we don't know what names it will introduce,
// so this stubs macro must be "names: arbitrary":
// CHECK: @freestanding(declaration, names: arbitrary)
// CHECK: macro _distributed_stubs_G3() =
// CHECK:   #distributedStubs(
// CHECK:     module: "main", protocolName: "G3",
// CHECK:     stubProtocols: ["EmptyBase"],
// CHECK:     "distributed func get() -> String",
// CHECK:     "distributed func greet(name: String) -> String"
// CHECK:   )
//
// TODO: distributed actor $G3<ActorSystem>: Greeter where SerializationRequirement == any Codable {
// CHECK: distributed actor $G3: G3, EmptyBase {
// TODO:    Preferably, we could refer to our own macro like this: #_distributed_stubs_G3
// WORKAROUND:
// CHECK:   #distributedStubs(
// CHECK:      module: "main", protocolName: "G3",
// CHECK:      stubProtocols: ["EmptyBase"],
// CHECK:      "distributed func get() -> String",
// CHECK:      "distributed func greet(name: String) -> String"
// CHECK:    )
// CHECK:
// FIXME: the below cannot find the macro because it's form the same module
// CHECK:    // stub inherited members
// CHECK:    #_distributed_stubs_EmptyBase
// CHECK: }

// ==== ------------------------------------------------------------------------
