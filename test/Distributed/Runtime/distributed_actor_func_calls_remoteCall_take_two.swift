// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
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

final class SomeClass: Sendable, Codable {}

distributed actor Greeter {
  distributed func take(name: String, int: Int) {
    print("take: \(name), int: \(int)")
  }

  distributed func take(name: String, int: Int, clazz: SomeClass) {
    print("take: \(name), int: \(int), clazz: \(clazz)")
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(system: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  try await ref.take(name: "Caplin", int: 1337)
  // CHECK: >> remoteCallVoid: on:main.Greeter, target:main.Greeter.take(name:int:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Caplin", 1337], returnType: nil, errorType: nil), throwing:Swift.Never

  // try await ref.take(name: "Caplin", int: 1337, clazz: .init()) // FIXME(distributed): crashes

}

@main struct Main {
  static func main() async {
    try! await test()
  }
}

