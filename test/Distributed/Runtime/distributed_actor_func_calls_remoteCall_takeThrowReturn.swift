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
  distributed func takeThrowReturn(name: String) async throws -> String {
    throw SomeError()
  }
}

struct SomeError: Error, Sendable, Codable {}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  do {
    let value = try await ref.takeThrowReturn(name: "Example")
    // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.takeThrowReturn(name:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Example"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String

    print("did not throw")
    // CHECK-NOT: did not throw
  } catch {
    // CHECK: << onThrow: SomeError()
    // CHECK: << remoteCall throw: SomeError()
    print("error: \(error)")
    // CHECK: error: SomeError()
  }
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
