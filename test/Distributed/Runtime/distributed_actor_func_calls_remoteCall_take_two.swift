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

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

final class SomeClass: Sendable, Codable {}

distributed actor Greeter {
  distributed func take(name: String, int: Int) {
    print("take: \(name), int: \(int)")
  }

  distributed func take(name: String, int: Int, clazz: SomeClass) {
    print("take: \(name), int: \(int), clazz: \(clazz)")
  }

  distributed func params(param p1: String, param p2: Int) -> String {
    let message = "params: p1: \(p1), p2: \(p2)"
    print(message)
    return message
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  try await ref.take(name: "Caplin", int: 1337)
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.take(name:int:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Caplin", 1337], returnType: nil, errorType: nil), throwing:Swift.Never

  try await ref.take(name: "Caplin", int: 1337, clazz: .init())

  let r3 = try await ref.params(param: "one", param: 2)
  print("r3 = \(r3)") // CHECK: r3 = params: p1: one, p2: 2
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}

