// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol KappaProtocol : DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func echo(name: String) -> String
}

distributed actor KappaProtocolImpl: KappaProtocol {
  // empty, gets default impl from extension on this actor
}

extension KappaProtocolImpl {
  distributed func echo(name: String) -> String {
    return "Echo: \(name)"
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = KappaProtocolImpl(actorSystem: system)
  let ref = try KappaProtocolImpl.resolve(id: local.id, using: system)

  let reply = try await ref.echo(name: "Caplin")
  // CHECK: >> remoteCall: on:main.KappaProtocolImpl, target:main.KappaProtocolImpl.echo(name:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Caplin"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String

  // CHECK: << remoteCall return: Echo: Caplin
  print("reply: \(reply)")
  // CHECK: reply: Echo: Caplin
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
