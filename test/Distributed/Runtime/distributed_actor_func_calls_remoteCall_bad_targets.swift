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

distributed actor Greeter {
  distributed func greet(name: String) -> String {
      "Hello, \(name)!"
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()
  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  // Make sure normal call works ok:
  let greeting = try await ref.greet(name: "Caplin")
  print("\(greeting)")
  // CHECK: Hello, Caplin!

  let correctTargetIdentifier = "$s4main7GreeterC5greet4nameS2S_tYaKFTE"
  _ = correctTargetIdentifier
  let badModuleTargetIdentifier = "$s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE"
  // the BADMODULE is a bad module, and we won't be able to find the distributed accessor
  // this should result in a failed call, but not hang the call.

  var invocation = Greeter.ActorSystem.InvocationEncoder()
  invocation.arguments = ["BadCall"]
  invocation.returnType = String.self

  let badTarget = RemoteCallTarget(badModuleTargetIdentifier)

  do {
    // CHECK: >> remoteCall: on:main.Greeter, target:BADMODULE.Greeter.greet(name:)
    _ = try await system.remoteCall(
      on: local,
      target: badTarget,
      invocation: &invocation,
      throwing: Never.self,
      returning: String.self
    )
  } catch {
    // CHECK: << onThrow: ExecuteDistributedTargetError(errorCode: Distributed.ExecuteDistributedTargetError.ErrorCode.targetAccessorNotFound, message: "Failed to locate distributed function accessor")
    // CHECK: << remoteCall throw: ExecuteDistributedTargetError(errorCode: Distributed.ExecuteDistributedTargetError.ErrorCode.targetAccessorNotFound, message: "Failed to locate distributed function accessor")
    print("caught error: \(error)")
    print("call target was: \(badTarget.identifier)")
    // CHECK: caught error: ExecuteDistributedTargetError(errorCode: Distributed.ExecuteDistributedTargetError.ErrorCode.targetAccessorNotFound, message: "Failed to locate distributed function accessor")
    // CHECK: call target was: $s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE
  }
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
