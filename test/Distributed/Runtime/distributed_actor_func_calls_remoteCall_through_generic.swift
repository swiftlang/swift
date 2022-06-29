// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color --dump-input=always

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

protocol DistributedWorker: DistributedActor where ActorSystem == DefaultDistributedActorSystem {
  associatedtype WorkItem: Sendable & Codable
  associatedtype WorkResult: Sendable & Codable

  distributed func submit_witness_sync(work: WorkItem) -> WorkResult
//  distributed func submit_witness_throws(work: WorkItem) throws -> WorkResult
//  distributed func submit_witness_async(work: WorkItem) async -> WorkResult
//  distributed func submit_witness_asyncThrows(work: WorkItem) async throws -> WorkResult

  // non distributed requirements can be witnessed with _normal_ functions
//  func sync(work: WorkItem) -> WorkResult
//  func async(work: WorkItem) async -> WorkResult
//  func syncThrows(work: WorkItem) throws -> WorkResult
//  func asyncThrows(work: WorkItem) async throws -> WorkResult
}

distributed actor TheWorker: DistributedWorker {
  typealias ActorSystem = DefaultDistributedActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func submit_witness_sync(work: WorkItem) -> WorkResult {
    "\(#function): \(work)"
  }

}

func test_generic(system: DefaultDistributedActorSystem) async throws {
  let localW = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: localW.id, using: system)
  precondition(__isRemoteActor(remoteW))

  print("=== -------------------------------------------------------")

  // === sync witness ------

  func callWorkerSync<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.submit_witness_sync(work: "Hello")
  }
  let replySync = try await callWorkerSync(w: remoteW)
  print("submit_witness_sync (remote): \(replySync)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_sync(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: << remoteCall return: submit_witness_sync(work:): Hello
  // CHECK: submit_witness_sync (remote): submit_witness_sync(work:): Hello

  let replyLocal = try await callWorkerSync(w: localW)
  print("submit_witness_sync (local): \(replyLocal)")
  // CHECK-NOT: >> remoteCall
  // CHECK-NEXT: submit_witness_sync (local): submit_witness_sync(work:): Hello
  print("=== -------------------------------------------------------")


}

func test_whenLocal(system: DefaultDistributedActorSystem) async throws {
}

@main struct Main {
  static func main() async {
    let system = DefaultDistributedActorSystem()
    try! await test_generic(system: system)
    print("==== ---------------------------------------------------")
    try! await test_whenLocal(system: system)
  }
}
