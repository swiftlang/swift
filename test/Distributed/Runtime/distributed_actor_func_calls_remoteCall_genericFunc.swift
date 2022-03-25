// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: windows

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
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
