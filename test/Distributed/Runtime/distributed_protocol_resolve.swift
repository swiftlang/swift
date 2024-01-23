// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -parse-as-library -Xfrontend -dump-macro-expansions -Xfrontend -disable-availability-checking -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// FIXME: reworking the resolve, maybe no need for a macro
// XFAIL: *

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol Greeter: DistributedActor {
  distributed func greet(name: String) -> String
}

// FIXME: generate these macros
@freestanding(declaration, names: named(greet(name:)))
macro _distributed_stubs_Greeter() =
  #distributedStubs(
    module: "main", protocolName: "Greeter",
    "distributed func greet(name: String) -> String"
  )

@freestanding(expression)
macro resolve_Greeter(
// macro resolve_Greeter<DAS: DistributedActorSystem>(  // FIXME: generics issue
  stubName: String, id: Any, using system: Any
) -> Any =
  #distributedResolveStub(
    stubTypeName: stubName,
    module: "Module", protocolName: "Greeter",
    actorSystemName: "FakeRoundtripActorSystem", // FIXME: workaround
    id: id, using: system,
    stubProtocols: "Greeter"
  )

distributed actor Impl: Greeter {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func greet(name: String) -> String{
    "Hello, \(name)!"
  }
}

@available(SwiftStdlib 5.7, *)
@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()
    let impl = Impl(actorSystem: system)

    let resolved: any Greeter =
      // try #resolve<any Greeter, FakeRoundtripActorSystem>(id: impl.id, using: system) // FIXME: generics issue
      try #resolve<any Greeter>(systemName: "FakeRoundtripActorSystem", id: impl.id, using: system) // FIXME: generics issue

    assert(resolved.id == impl.id)
  }
}
