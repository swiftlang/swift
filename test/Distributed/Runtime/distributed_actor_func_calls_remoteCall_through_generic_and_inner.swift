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

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol Greeting: DistributedActor {
// protocol Greeting: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greeting() -> String
  distributed func greetingAsyncThrows() async throws -> String
}

extension Greeting {
  func greetLocal(name: String) async throws {
    try await print("\(greetingAsyncThrows()), \(name)!") // requirement is async throws, things work
  }

  func greetLocal2(name: String) {
    print("\(greeting()), \(name)!")
  }
}

extension Greeting where ActorSystem: DistributedActorSystem<any Codable> {
  // okay, uses Codable to transfer arguments.
  distributed func greetDistributed(name: String) async throws {
    // okay, we're on the actor
    try await greetLocal(name: name)
  }

  distributed func greetDistributed2(name: String) async throws {
    // okay, we're on the actor
    greetLocal2(name: name)
  }

  func greetDistributedNon(name: String) async throws {
    // okay, we're on the actor
    greetLocal2(name: name)
  }
}

extension Greeting where ActorSystem: DistributedActorSystem<any Codable> {
  nonisolated func greetAliceALot() async throws {
    try await greetDistributed(name: "Alice") // okay, via Codable
    let rawGreeting = try await greeting() // okay, via Self's serialization requirement
    print("rawGreeting = \(rawGreeting)")
    // greetLocal(name: "Alice") // would be error: only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  }
}

distributed actor Greeter: Greeting {
  distributed func greeting() -> String {
    "Hello"
  }

  distributed func greetingAsyncThrows() -> String {
    noop() // no await needed, we're in the same actor
    return "Hello from AsyncThrows"
  }

  func callNoop() {
    noop() // no await needed, we're in the same actor
  }

  distributed func noop() {
    // nothing
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()
  let g = Greeter(actorSystem: system)

  let greeting = try await g.greeting()
  print("greeting(): \(greeting)") // CHECK: greeting(): Hello

  try await g.greetDistributed(name: "Caplin")
  // CHECK: Hello from AsyncThrows, Caplin!

  try await g.greetDistributed2(name: "Caplin")
  // CHECK: Hello, Caplin!
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
