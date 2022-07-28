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
    let call = try await system.remoteCall(
      on: local,
      target: badTarget,
      invocation: &invocation,
      throwing: Never.self,
      returning: String.self
    )
  } catch {
    // CHECK: << onThrow: ExecuteDistributedTargetError(message: "Could not find distributed accessor for target $s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE")
    // CHECK: << remoteCall throw: ExecuteDistributedTargetError(message: "Could not find distributed accessor for target $s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE")
    print("caught error: \(error)")
    // CHECK: caught error: ExecuteDistributedTargetError(message: "Could not find distributed accessor for target $s9BADMODULE7GreeterC5greet4nameS2S_tYaKFTE")
  }
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
