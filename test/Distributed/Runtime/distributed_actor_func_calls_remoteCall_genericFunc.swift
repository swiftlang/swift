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

distributed actor Greeter {
  distributed func generic<V: Codable>(_ value: V) -> String {
    _ = V.self
    return "\(value)"
  }

  distributed func generic2<A: Codable, B: Codable>(
      strict: Double, _ value: A, _ bs: [B]) -> String {
    return "\(value) \(bs)"
  }

}

distributed actor GenericGreeter<Greeting: Sendable & Codable> {
  distributed func echo(greeting: Greeting) -> Greeting {
    greeting
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  let r1 = try await ref.generic("Caplin")
  // CHECK: > encode generic sub: Swift.String
  // CHECK: > encode argument name:_, value: Caplin
  // CHECK: > encode return type: Swift.String
  // CHECK: > done recording
  // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.generic(_:), invocation:FakeInvocationEncoder(genericSubs: [Swift.String], arguments: ["Caplin"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  print("reply: \(r1)")
  // CHECK: reply: Caplin

  let r2 = try await ref.generic2(
      strict: 2.0,
      "Caplin",
      [1, 2, 3]
  )
  // CHECK: > encode generic sub: Swift.String
  // CHECK: > encode generic sub: Swift.Int
  // CHECK: > encode argument name:strict, value: 2.0
  // CHECK: > encode argument name:_, value: Caplin
  // CHECK: > encode argument name:_, value: [1, 2, 3]
  // CHECK: > encode return type: Swift.String
  // CHECK: > done recording
  // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.generic2(strict:_:_:), invocation:FakeInvocationEncoder(genericSubs: [Swift.String, Swift.Int], arguments: [2.0, "Caplin", [1, 2, 3]], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  print("reply: \(r2)")
  // CHECK: reply: Caplin

  let gen = GenericGreeter<String>(actorSystem: system)
  let r3 = try await gen.echo(greeting: "Hello generics!")
  print("reply: \(r3)")
  // CHECK: reply: Hello generics!

  let genRef = try GenericGreeter<String>.resolve(id: gen.id, using: system)
  let r32 = try await genRef.echo(greeting: "Hello generics!")
  // CHECK: > encode generic sub: Swift.String
  // CHECK: > encode return type: Swift.String
  print("reply: \(r32)")
  // CHECK: reply: Hello generics!
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
