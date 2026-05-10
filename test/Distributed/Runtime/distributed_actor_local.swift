// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems

distributed actor SomeSpecificDistributedActor {

  distributed func hello() async throws {
     print("hello from \(self.id)")
  }

  distributed func echo(int: Int) async throws -> Int {
    int
  }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test_initializers() {
  let address = ActorAddress(parse: "")
  let system = DefaultDistributedActorSystem()

  _ = SomeSpecificDistributedActor(actorSystem: system)
  _ = try! SomeSpecificDistributedActor.resolve(id: address, using: system)
}

func test_address() {
  let system = DefaultDistributedActorSystem()

  let actor = SomeSpecificDistributedActor(actorSystem: system)
  _ = actor.id
}

func test_run(system: FakeActorSystem) async {
  let actor = SomeSpecificDistributedActor(actorSystem: system)

  print("before") // CHECK: before
  try! await actor.hello()
  print("after") // CHECK: after
}

func test_echo(system: FakeActorSystem) async {
  let actor = SomeSpecificDistributedActor(actorSystem: system)

  let echo = try! await actor.echo(int: 42)
  print("echo: \(echo)") // CHECK: echo: 42
}

@main struct Main {
  static func main() async {
    await test_run(system: FakeActorSystem())
    await test_echo(system: FakeActorSystem())
  }
}
