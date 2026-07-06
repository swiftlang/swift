// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx13.0 -parse-as-library -emit-library -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift -o %t/%target-library-name(FakeDistributedActorSystems)
// RUN: %target-build-swift -target %target-cpu-apple-macosx13.0 -parse-as-library -lFakeDistributedActorSystems -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
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

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@available(SwiftStdlib 5.7, *)
distributed actor Greeter {
  distributed func hello() -> String {
    return "Hello, World!"
  }
}

@available(SwiftStdlib 5.7, *)
func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  let response = try await ref.hello()
  // CHECK: >> remoteCall: on:main.Greeter, target:main.Greeter.hello(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String

  print("response: \(response)")
  // CHECK: response: Hello, World!

}

@available(SwiftStdlib 5.7, *)
@main struct Main {
  static func main() async {
    try! await test()
  }
}
