// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
//
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// This test exercises the **remote-branch** wire round-trip for a
// `distributed func` that takes a `some/any P` parameter where `P` is a
// `@Resolvable protocol`, and for a `distributed func` that *returns* `any P`.

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@Resolvable
protocol Greeter: DistributedActor, Codable
where ActorSystem == FakeRoundtripActorSystem {
  distributed func sayHi() -> String
  distributed func sendAnyGreeter(_ g: any Greeter) async throws -> String
  distributed func sendSomeGreeter(_ g: some Greeter) async throws -> String
  distributed func echoActor(_ g: any Greeter) async throws -> any Greeter
  distributed var currentSelf: any Greeter { get }
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

  distributed func echoActor(_ g: any Greeter) async throws -> any Greeter {
    print("echoActor type: \(type(of: g))")
    return g
  }

  distributed var currentSelf: any Greeter {
    // FakeRoundtripActorSystem passes values by reference; resolve as $Greeter
    // so the caller receives a remote proxy, not the local GreeterImpl instance.
    return try! $Greeter.resolve(id: id, using: actorSystem)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

func test_anyGreeter() async throws {
  let system = FakeRoundtripActorSystem()
  let local = GreeterImpl(actorSystem: system)
  let proxy = try GreeterImpl.resolve(id: local.id, using: system)

  print("any Greeter")
  // CHECK-LABEL: any Greeter
  let result = try await proxy.sendAnyGreeter(local)
  // CHECK: sendAnyGreeter type: $Greeter
  print("result: \(result)")
  // CHECK: result: Hi from
}

func test_someGreeter() async throws {
  let system = FakeRoundtripActorSystem()
  let local = GreeterImpl(actorSystem: system)
  let proxy = try GreeterImpl.resolve(id: local.id, using: system)

  print("some Greeter")
  // CHECK-LABEL: some Greeter
  let result = try await proxy.sendSomeGreeter(local)
  // CHECK: sendSomeGreeter type: $Greeter
  print("result: \(result)")
  // CHECK: result: Hi from
}

func test_echoActor() async throws {
  let system = FakeRoundtripActorSystem()
  let local = GreeterImpl(actorSystem: system)
  let proxy = try GreeterImpl.resolve(id: local.id, using: system)

  print("echo any Greeter -> any Greeter")
  // CHECK-LABEL: echo any Greeter -> any Greeter
  let echoed = try await proxy.echoActor(local)
  // CHECK: echoActor type: $Greeter
  print("echoed type: \(type(of: echoed))")
  // CHECK: echoed type: $Greeter
  let result = try await echoed.sayHi()
  print("result: \(result)")
  // CHECK: result: Hi from
}

func test_currentSelf() async throws {
  let system = FakeRoundtripActorSystem()
  let local = GreeterImpl(actorSystem: system)
  let proxy = try GreeterImpl.resolve(id: local.id, using: system)

  print("var currentSelf -> any Greeter")
  // CHECK-LABEL: var currentSelf -> any Greeter
  let got = try await proxy.currentSelf
  print("currentSelf type: \(type(of: got))")
  // CHECK: currentSelf type: $Greeter
  let result = try await got.sayHi()
  print("result: \(result)")
  // CHECK: result: Hi from
}

// ==== -----------------------------------------------------------------------
// MARK: Main

@main struct Main {
  static func main() async throws {
    try await test_anyGreeter()
    try await test_someGreeter()
    try await test_echoActor()
    try await test_currentSelf()
  }
}
