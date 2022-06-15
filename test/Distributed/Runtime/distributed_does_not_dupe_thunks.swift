// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -emit-library -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift -o %t/%target-library-name(FakeDistributedActorSystems)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -lFakeDistributedActorSystems -module-name main -I %t -L %t %s -emit-sil
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
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

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@available(SwiftStdlib 5.7, *)
func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Kappa(actorSystem: system)

  let response = try await local.theFunction()
  // CHECK: >> remoteCall: on:main.Kappa, target:main.Greeter.hello(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String

  print("response: \(response)")
  // CHECK: response: hello
}

@available(SwiftStdlib 5.7, *)
@main struct Main {
  static func main() async {
    try! await test()
  }
}
