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

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor MyActor {
  distributed var computed: String {
    "Hello"
  }

  distributed func bar(other: MyActor) async throws {
    print("print inside bar(other:)")
    print(try await other.computed)
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = MyActor(actorSystem: system)
  let res = try await local.computed
  print("local response: \(res)")
  // CHECK: local response: Hello

  let ref = try MyActor.resolve(id: local.id, using: system)
  try await ref.computed
  // CHECK: >> remoteCall: on:main.MyActor, target:main.MyActor.computed(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: << onReturn: Hello

  try await MyActor(actorSystem: system).bar(other: ref)
  // CHECK: print inside bar(other:)
  // CHECK: Hello
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
