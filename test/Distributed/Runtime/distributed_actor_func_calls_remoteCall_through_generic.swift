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

  distributed func submit(work: WorkItem) async throws -> WorkResult

  func sync(work: WorkItem) -> WorkResult
}

distributed actor TheWorker: DistributedWorker {
  typealias ActorSystem = DefaultDistributedActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func submit(work: WorkItem) async throws -> WorkResult {
    "\(Self.self) echo: \(work)"
  }

  func sync(work: WorkItem) -> WorkResult {
    return "SYNC: \(work)"
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let w = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: w.id, using: system)
  print("remoteW is remote: \(__isRemoteActor(remoteW))")
  // CHECK: remoteW is remote: true

  // direct calls work ok:
  let replyDirect = try await remoteW.submit(work: "Direct")
  print("reply direct: \(replyDirect)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: reply direct: TheWorker echo: Direct

  print("==== ---------------------------------------------------")

  func callWorker<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.submit(work: "Hello")
  }
  let reply = try await callWorker(w: remoteW)

  print("reply: \(reply)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: << remoteCall return: TheWorker echo: Hello
  // CHECK: reply: TheWorker echo: Hello

  // ----------
  let replySyncRemote = await remoteW.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.sync(work: "test")
  }
  print("reply sync (remote): \(replySyncRemote)")
  // CHECK: reply sync (remote): nil

  let replySyncLocal = await w.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.sync(work: "test")
  }
  print("reply sync (local): \(replySyncLocal)")
  // CHECK: reply sync (local): Optional("SYNC: test")
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
