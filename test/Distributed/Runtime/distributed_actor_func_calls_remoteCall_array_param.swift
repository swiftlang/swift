// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -plugin-path %swift-plugin-dir -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -plugin-path %swift-plugin-dir -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --dump-input=always

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

@Resolvable
@available(SwiftStdlib 6.0, *)
protocol Service: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func getArray(a1: [Int], a2: String?) async throws -> [Int]

  // Make sure method-level generics also work fine
  distributed func getArrayGeneric<Gen: Codable & Sendable>(a1: [Int], a2: String?, gen: Gen) async throws -> [Int]
}

@available(SwiftStdlib 6.0, *)
distributed actor ServiceImpl: Service {
  distributed func getArray(a1: [Int], a2: String?) async throws -> [Int] {
    return a1 + [4, 5]
  }

  distributed func getArrayGeneric<Gen: Codable & Sendable>(a1: [Int], a2: String?, gen: Gen) async throws -> [Int] {
    return a1 + [4, 5]
  }
}

@available(SwiftStdlib 6.0, *)
func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = ServiceImpl(actorSystem: system)

  let implRef = try ServiceImpl.resolve(id: local.id, using: system)
  let r1 = try await implRef.getArray(a1: [1, 2, 3], a2: "second")
  // CHECK: >> remoteCall: on:main.ServiceImpl, target:main.ServiceImpl.getArray(a1:a2:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [[[ARGS:.*]]], returnType: Optional(Swift.Array<Swift.Int>), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.Array<Swift.Int>
  // CHECK: > execute distributed target: main.ServiceImpl.getArray(a1:a2:), identifier: $s4main11ServiceImplC8getArray2a12a2SaySiGAG_SSSgtYaKFTE
  print("reply 1: \(r1)")
  // CHECK: reply 1: [1, 2, 3, 4, 5]

  let ref = try $Service.resolve(id: local.id, using: system)
  let r2 = try await ref.getArray(a1: [1, 2, 3], a2: "second")
  // CHECK: >> remoteCall: on:main.$Service, target:main.$Service.getArray(a1:a2:), invocation:FakeInvocationEncoder(genericSubs: [main.$Service], arguments: [[[ARGS:.*]]], returnType: Optional(Swift.Array<Swift.Int>), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.Array<Swift.Int>
  // CHECK: > execute distributed target: main.$Service.getArray(a1:a2:), identifier: $s4main8$ServiceC8getArray2a12a2SaySiGAG_SSSgtYaKFTE
  // CHECK: > decode generic subs: [main.$Service]
  // CHECK: > decode return type: Swift.Array<Swift.Int>
  // CHECK: > decode argument: [1, 2, 3]
  // CHECK: > decode argument: Optional("second")
  print("reply 2: \(r2)")
  // CHECK: reply 2: [1, 2, 3, 4, 5]

  _ = try await ref.getArrayGeneric(a1: [1, 2, 3], a2: "third", gen: 12)
  // CHECK: remoteCall: on:main.$Service, target:main.$Service.getArrayGeneric(a1:a2:gen:), invocation:FakeInvocationEncoder(genericSubs: [main.$Service, Swift.Int], arguments: [[[ARGS:.*]]], returnType: Optional(Swift.Array<Swift.Int>), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.Array<Swift.Int>
  // CHECK: > execute distributed target: main.$Service.getArrayGeneric(a1:a2:gen:), identifier: $s4main8$ServiceC15getArrayGeneric2a12a23genSaySiGAH_SSSgqd__tYaKSeRd__SERd__lFTE
  // CHECK: > decode generic subs: [main.$Service, Swift.Int]
  // CHECK: > decode return type: Swift.Array<Swift.Int>
  // CHECK: > decode argument: [1, 2, 3]
  // CHECK: > decode argument: Optional("third")
  // CHECK: > decode argument: 12
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    try! await test()

    print("Done")
    // CHECK: Done
  }
}
