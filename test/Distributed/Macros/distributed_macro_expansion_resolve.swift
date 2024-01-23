// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s 2>&1 | %FileCheck %s --dump-input=always

// FIXME: hopefully we may not need #resolve
// XFAIL: *

import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

protocol GreetingsProtocol: DistributedActor {
  distributed func greet(name: String) -> String
}

// ==== ------------------------------------------------------------------------

// FIXME: generate these macros
@freestanding(declaration, names: named(greet(name:)))
macro GreetingsProtocol_stubs() =
  #distributedStubs(
    module: "main", protocolName: "Greeter",
    "distributed func greet(name: String) -> String"
  )

@freestanding(expression)
macro resolve_GreetingsProtocol<DAS: DistributedActorSystem>(
  stubName: String,
  id someID: DAS.ActorID,
  using someSystem: DAS) -> Any =
    #distributedResolveStub<DAS>(
      stubTypeName: stubName,
      module: "main", protocolName: "GreetingsProtocol",
      id: someID, using: someSystem,
      stubProtocols: "GreetingsProtocol"
    )

// ==== ------------------------------------------------------------------------


func test(id: LocalTestingDistributedActorSystem.ActorID,
          system: LocalTestingDistributedActorSystem) throws {
  let _: any GreetingsProtocol =
    try #resolve<any GreetingsProtocol, LocalTestingDistributedActorSystem>(id: id, using: system)
}
// CHECK: #resolve_GreetingsProtocol<LocalTestingDistributedActorSystem>(
// CHECK:  stubName: "[[STUB_NAME:.*]]",
// CHECK:  id: id,
// CHECK:  using: system
// CHECK: ) as! (any GreetingsProtocol)


// CHECK: {
// CHECK:   distributed actor [[STUB_NAME:$.+]]: GreetingsProtocol {
// CHECK:     typealias ActorSystem = LocalTestingDistributedActorSystem
// CHECK:     #GreetingsProtocol_stubs
// CHECK:   }
// CHECK:   return try [[STUB_NAME]].resolve(
// CHECK:     id: id,
// CHECK:     using: system
// CHECK:   )
// CHECK: }() as! (any GreetingsProtocol)



