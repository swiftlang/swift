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

protocol DistributedWorker: DistributedActor {
  associatedtype WorkItem: Sendable & Codable
  associatedtype WorkResult: Sendable & Codable

  distributed func submit(work: WorkItem) async throws -> WorkResult
}

distributed actor TheWorker: DistributedWorker {
  typealias ActorSystem = DefaultDistributedActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func submit(work: WorkItem) async throws -> WorkResult {
    "\(Self.self) echo: \(work)"
  }
}

actor WorkerPool<Worker: DistributedWorker> {
  typealias ActorSystem = FakeRoundtripActorSystem
  typealias WorkItem = Worker.WorkItem
  typealias WorkResult = Worker.WorkResult

  let actorSystem: ActorSystem
  let worker: Worker

  init(worker: Worker, actorSystem: ActorSystem) {
    self.worker = worker
    self.actorSystem = actorSystem
  }

  func submit(work: WorkItem) async throws -> WorkResult {
    let worker = try await self.selectWorker()
    return try await worker.submit(work: work)
  }

  func selectWorker() async throws -> Worker {
    return self.worker
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let w = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: w.id, using: system)
  print("remoteW is remote: \(__isRemoteActor(remoteW))")

  let reply = try await remoteW.submit(work: "Hello")

//  let pool = WorkerPool<TheWorker>(worker: remoteW, actorSystem: system)
//  let reply = try await pool.submit(work: "Hello")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String

  // CHECK: << remoteCall return: TheWorker echo: Hello
  print("reply: \(reply)")
  // CHECK: reply: TheWorker echo: Hello
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
