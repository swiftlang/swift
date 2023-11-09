// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

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
  distributed func callMe(_ name: String) -> String {
    return "\(name)"
  }
  distributed func callMe(_ number: Int) -> String {
    return "\(number)"
  }
}

struct SomeError: Error, Sendable, Codable {}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  do {
    let echo = try await ref.callMe("hello")
    precondition(echo == "hello")
    // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.callMe(_:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["hello"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String

    let echo2 = try await ref.callMe(42)
    precondition(echo2 == "42")
    // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.callMe(_:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [42], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String

    print("did not throw")
    // CHECK: did not throw
  } catch {
    print("error: \(error)")
    // CHECK-NOT: error:
  }
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
