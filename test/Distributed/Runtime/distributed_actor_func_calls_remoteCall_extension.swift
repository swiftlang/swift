// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --dump-input=always

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

extension FakeRoundtripActorSystemDistributedActor {
  distributed func echo(name: String) -> String {
    return "Echo: \(name)"
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = FakeRoundtripActorSystemDistributedActor(actorSystem: system)
  let ref = try FakeRoundtripActorSystemDistributedActor.resolve(id: local.id, using: system)

  let reply = try await ref.echo(name: "Caplin")
  // CHECK: >> remoteCall: on:main.FakeRoundtripActorSystemDistributedActor, target:main.FakeRoundtripActorSystemDistributedActor.echo(name:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Caplin"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String

  // CHECK: << remoteCall return: Echo: Caplin
  print("reply: \(reply)")
  // CHECK: reply: Echo: Caplin
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
