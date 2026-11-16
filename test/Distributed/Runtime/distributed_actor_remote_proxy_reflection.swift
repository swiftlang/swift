// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://188894010
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

struct Blob {
  var a: UInt64 = 0x1111111111111111
  var b: UInt64 = 0x1111111111111111
  var c: UInt64 = 0x1111111111111111
  var d: UInt64 = 0x1111111111111111
  var e: UInt64 = 0x1111111111111111
  var f: UInt64 = 0x1111111111111111
  var g: UInt64 = 0x1111111111111111
  var h: UInt64 = 0x1111111111111111
}

distributed actor Node {
  typealias ActorSystem = DefaultDistributedActorSystem

  var blob = Blob()
  var tail: UInt64 = 0x2222222222222222

  distributed func ping() {}
}

/// A distributed actor with a custom executor is not a default actor, so its
/// remote flag lives in `NonDefaultDistributedActor` rather than in the default
/// actor state.
final class InlineExecutor: SerialExecutor {
  func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: asUnownedSerialExecutor())
  }
}

let sharedInlineExecutor = InlineExecutor()

distributed actor CustomExecutorNode {
  typealias ActorSystem = DefaultDistributedActorSystem

  var blob = Blob()
  var tail: UInt64 = 0x2222222222222222

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    sharedInlineExecutor.asUnownedSerialExecutor()
  }

  distributed func ping() {}
}

func test() async throws {
  let system = DefaultDistributedActorSystem()
  let local = Node(actorSystem: system)
  let remote = try Node.resolve(id: local.id, using: system)
  print("isRemote: \(__isRemoteActor(remote))")
  // CHECK: isRemote: true

  // A remote proxy is allocated without storage for user-defined stored
  // properties, so reflection must only enumerate the synthesized ones.
  let remoteLabels = Mirror(reflecting: remote).children.map { $0.label ?? "?" }
  print("remote children: \(remoteLabels.count)")
  // CHECK: remote children: 3
  print("remote has blob: \(remoteLabels.contains("blob")) tail: \(remoteLabels.contains("tail"))")
  // CHECK: remote has blob: false tail: false

  // Dumping must not read past the allocation either.
  dump(remote)

  // Local instances still reflect every field.
  let localLabels = Mirror(reflecting: local).children.map { $0.label ?? "?" }
  print("local has blob: \(localLabels.contains("blob")) tail: \(localLabels.contains("tail"))")
  // CHECK: local has blob: true tail: true
}

func testCustomExecutor() async throws {
  let system = DefaultDistributedActorSystem()
  let local = CustomExecutorNode(actorSystem: system)
  let remote = try CustomExecutorNode.resolve(id: local.id, using: system)
  print("custom isRemote: \(__isRemoteActor(remote))")
  // CHECK: custom isRemote: true

  let remoteLabels = Mirror(reflecting: remote).children.map { $0.label ?? "?" }
  print("custom remote children: \(remoteLabels.count)")
  // CHECK: custom remote children: 3
  print("custom remote has blob: \(remoteLabels.contains("blob")) tail: \(remoteLabels.contains("tail"))")
  // CHECK: custom remote has blob: false tail: false

  dump(remote)

  let localLabels = Mirror(reflecting: local).children.map { $0.label ?? "?" }
  print("custom local has blob: \(localLabels.contains("blob")) tail: \(localLabels.contains("tail"))")
  // CHECK: custom local has blob: true tail: true
}

@main struct Main {
  static func main() async {
    try! await test()
    try! await testCustomExecutor()
  }
}
