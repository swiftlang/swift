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

  // distributed requirement currently is forced to be `async throws`...
  // FIXME(distributed): requirements don't have to be async throws,
  //                     distributed makes them implicitly async throws anyway...
  distributed func submit(work: WorkItem) async throws -> WorkResult

  // non distributed requirements can be witnessed with _normal_ functions
  func sync(work: WorkItem) -> WorkResult
  func async(work: WorkItem) async -> WorkResult
  func syncThrows(work: WorkItem) throws -> WorkResult
  func asyncThrows(work: WorkItem) async throws -> WorkResult
}

distributed actor TheWorker: DistributedWorker {
  typealias ActorSystem = DefaultDistributedActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func submit(work: WorkItem) async throws -> WorkResult {
    "\(#function): \(work)"
  }

  func sync(work: WorkItem) -> WorkResult {
    return "\(#function): \(work)"
  }
  func async(work: WorkItem) async -> WorkResult {
    return "\(#function): \(work)"
  }
  func syncThrows(work: WorkItem) throws -> WorkResult {
    return "\(#function): \(work)"
  }
  func asyncThrows(work: WorkItem) async throws -> WorkResult {
    return "\(#function): \(work)"
  }
}

func test_generic(system: DefaultDistributedActorSystem) async throws {
  let localW = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: localW.id, using: system)
  precondition(__isRemoteActor(remoteW))

  // direct calls work ok:
  let replyDirect = try await remoteW.submit(work: "Direct")
  print("reply direct: \(replyDirect)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: reply direct: submit(work:): Direct

  func callWorker<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.submit(work: "Hello")
  }
  let reply = try await callWorker(w: remoteW)
  print("reply (remote): \(reply)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: << remoteCall return: submit(work:): Hello
  // CHECK: reply (remote): submit(work:): Hello

  let replyLocal = try await callWorker(w: localW)
  print("reply (local): \(replyLocal)")
  // CHECK-NOT: >> remoteCall
  // CHECK: reply (local): submit(work:): Hello
}

func test_whenLocal(system: DefaultDistributedActorSystem) async throws {
  let localW = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: localW.id, using: system)
  precondition(__isRemoteActor(remoteW))

  do {
    let replySync = await remoteW.whenLocal { __secretlyKnownToBeLocal in
      __secretlyKnownToBeLocal.sync(work: "test")
    }
    print("replySync (remote): \(replySync)")
    // CHECK: replySync (remote): nil

    let replySyncThrows = try await remoteW.whenLocal { __secretlyKnownToBeLocal in
      try __secretlyKnownToBeLocal.syncThrows(work: "test")
    }
    print("replySyncThrows (remote): \(replySyncThrows)")
    // CHECK: replySyncThrows (remote): nil

    let replyAsync = await remoteW.whenLocal { __secretlyKnownToBeLocal in
      await __secretlyKnownToBeLocal.async(work: "test")
    }
    print("replyAsync (remote): \(replyAsync)")
    // CHECK: replyAsync (remote): nil

    let replyAsyncThrows = try await remoteW.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrows(work: "test")
    }
    print("replyAsyncThrows (remote): \(replyAsyncThrows)")
    // CHECK: replyAsyncThrows (remote): nil
  }
  // ==== ----------------------------------------------------------------------

  do {
    let replyDistSubmit = try await localW.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.submit(work: "local-test")
    }
    print("replyDistSubmit (local): \(replyDistSubmit ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyDistSubmit (local): submit(work:): local-test

    let replySyncLocal = await localW.whenLocal { __secretlyKnownToBeLocal in
      __secretlyKnownToBeLocal.sync(work: "local-test")
    }
    print("replySyncLocal (local): \(replySyncLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replySyncLocal (local): sync(work:): local-test

    let replySyncThrowsLocal = try await localW.whenLocal { __secretlyKnownToBeLocal in
      try __secretlyKnownToBeLocal.syncThrows(work: "local-test")
    }
    print("replySyncThrowsLocal (local): \(replySyncThrowsLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replySyncThrowsLocal (local): syncThrows(work:): local-test

    let replyAsyncLocal = await localW.whenLocal { __secretlyKnownToBeLocal in
      await __secretlyKnownToBeLocal.async(work: "local-test")
    }
    print("replyAsyncLocal (local): \(replyAsyncLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyAsyncLocal (local): async(work:): local-test

    let replyAsyncThrowsLocal = try await localW.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrows(work: "local-test")
    }
    print("replyAsyncThrowsLocal (local): \(replyAsyncThrowsLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyAsyncThrowsLocal (local): asyncThrows(work:): local-test
  }
}

@main struct Main {
  static func main() async {
    let system = DefaultDistributedActorSystem()
    try! await test_generic(system: system)
    print("==== ---------------------------------------------------")
    try! await test_whenLocal(system: system)
  }
}
