// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --dump-input=always
//
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test exercises the **remote-branch** wire round-trip for a
// `distributed func` that takes a `some/any P` parameter where `P` is a
// `@Resolvable protocol`.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@Resolvable
protocol Greeter: DistributedActor, Codable
where ActorSystem == FakeRoundtripActorSystem {
  distributed func sayHi() -> String
  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String
  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String
}

distributed actor GreeterImpl: Greeter {
  distributed func sayHi() -> String { "Hi from \(self.id)" }

  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String {
    print("sendAnyGreeter type: \(type(of: g))")
    return try await g.sayHi()
  }

  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String {
    print("sendSomeGreeter type: \(type(of: g))")
    return try await g.sayHi()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

func test_anyGreeter() async throws {
  let system = FakeRoundtripActorSystem()
  let local = GreeterImpl(actorSystem: system)
  let proxy = try GreeterImpl.resolve(id: local.id, using: system)

  print("--- any ---")
  // CHECK: --- any ---
  let result = try await proxy.sendAnyGreeter(local)
  // CHECK: sendAnyGreeter type: $Greeter
  print("result: \(result)")
  // CHECK: result: Hi from
}

func test_someGreeter() async throws {
  let system = FakeRoundtripActorSystem()
  let local = GreeterImpl(actorSystem: system)
  let proxy = try GreeterImpl.resolve(id: local.id, using: system)

  print("--- some ---")
  // CHECK: --- some ---
  let result = try await proxy.sendSomeGreeter(local)
  // CHECK: sendSomeGreeter type: $Greeter
  print("result: \(result)")
  // CHECK: result: Hi from
}

// ==== -----------------------------------------------------------------------
// MARK: Main

@main struct Main {
  static func main() async throws {
    try await test_anyGreeter()
    try await test_someGreeter()
  }
}
