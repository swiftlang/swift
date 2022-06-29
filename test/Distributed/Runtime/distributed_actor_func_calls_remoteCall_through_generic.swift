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

protocol DistributedWorker: DistributedActor where ActorSystem == DefaultDistributedActorSystem {
  associatedtype WorkItem: Sendable & Codable
  associatedtype WorkResult: Sendable & Codable

  distributed func submit_witness_sync(work: WorkItem) -> WorkResult
  distributed func submit_witness_throws(work: WorkItem) -> WorkResult
  distributed func submit_witness_async(work: WorkItem) -> WorkResult
  distributed func submit_witness_asyncThrows(work: WorkItem) -> WorkResult

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

  distributed func submit_witness_sync(work: WorkItem) -> WorkResult {
    "\(#function): \(work)"
  }
  distributed func submit_witness_throws(work: WorkItem) throws -> WorkResult {
    "\(#function): \(work)"
  }
  distributed func submit_witness_async(work: WorkItem) async -> WorkResult {
    "\(#function): \(work)"
  }
  distributed func submit_witness_asyncThrows(work: WorkItem) async throws -> WorkResult {
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
  let replyDirect = try await remoteW.submit_witness_sync(work: "Direct")
  print("submit_witness_sync direct: \(replyDirect)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_sync(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: submit_witness_sync direct: submit_witness_sync(work:): Direct
  print("=== -------------------------------------------------------")

  let replyDirectAsync = try await remoteW.submit_witness_async(work: "Direct Async")
  print("submit_witness_async direct: \(replyDirectAsync)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_async(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct Async"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: submit_witness_async direct: submit_witness_async(work:): Direct Async
  print("=== -------------------------------------------------------")

  let replyDirectThrows = try await remoteW.submit_witness_throws(work: "Direct Throws")
  print("submit_witness_throws direct: \(replyDirectThrows)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_throws(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct Throws"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: submit_witness_throws direct: submit_witness_throws(work:): Direct Throws
  print("=== -------------------------------------------------------")

  let replyDirectAsyncThrows = try await remoteW.submit_witness_asyncThrows(work: "Direct Async Throws")
  print("submit_witness_asyncThrows direct: \(replyDirectAsyncThrows)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_asyncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct Async Throws"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: submit_witness_asyncThrows direct: submit_witness_asyncThrows(work:): Direct Async Throws
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

  // === async witness ------

  func callWorkerAsync<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.submit_witness_async(work: "Hello")
  }
  let replyAsync = try await callWorkerAsync(w: remoteW)
  print("submit_witness_async (remote): \(replyAsync)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_async(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
  // CHECK: << remoteCall return: submit_witness_async(work:): Hello
  // CHECK: submit_witness_async (remote): submit_witness_async(work:): Hello

  let replyLocalAsync = try await callWorkerAsync(w: localW)
  print("submit_witness_async (local): \(replyLocalAsync)")
  // CHECK-NOT: >> remoteCall
  // CHECK-NEXT: submit_witness_async (local): submit_witness_async(work:): Hello
  print("=== -------------------------------------------------------")

  // === throws witness ------

  func callWorkerThrows<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.submit_witness_throws(work: "Hello")
  }
  let replyThrows = try await callWorkerThrows(w: remoteW)
  print("submit_witness_throws (remote): \(replyThrows)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_throws(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: << remoteCall return: submit_witness_throws(work:): Hello
  // CHECK: submit_witness_throws (remote): submit_witness_throws(work:): Hello

  let replyLocalThrows = try await callWorkerThrows(w: localW)
  print("submit_witness_throws (local): \(replyLocalThrows)")
  // CHECK-NOT: >> remoteCall
  // CHECK-NEXT: submit_witness_throws (local): submit_witness_throws(work:): Hello
  print("=== -------------------------------------------------------")

  // === async throws witness ------

  func callWorkerAsyncThrows<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.submit_witness_asyncThrows(work: "Hello")
  }
  let replyAsyncThrows = try await callWorkerAsyncThrows(w: remoteW)
  print("submit_witness_asyncThrows (remote): \(replyAsyncThrows)")
  // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.submit_witness_asyncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
  // CHECK: << remoteCall return: submit_witness_asyncThrows(work:): Hello
  // CHECK: submit_witness_asyncThrows (remote): submit_witness_asyncThrows(work:): Hello

  let replyLocalAsyncThrows = try await callWorkerAsyncThrows(w: localW)
  print("submit_witness_asyncThrows (local): \(replyLocalAsyncThrows)")
  // CHECK-NOT: >> remoteCall
  // CHECK-NEXT: submit_witness_asyncThrows (local): submit_witness_asyncThrows(work:): Hello
  print("=== -------------------------------------------------------")
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
    print("=== -------------------------------------------------------")

    let replySyncThrows = try await remoteW.whenLocal { __secretlyKnownToBeLocal in
      try __secretlyKnownToBeLocal.syncThrows(work: "test")
    }
    print("replySyncThrows (remote): \(replySyncThrows)")
    // CHECK: replySyncThrows (remote): nil
    print("=== -------------------------------------------------------")

    let replyAsync = await remoteW.whenLocal { __secretlyKnownToBeLocal in
      await __secretlyKnownToBeLocal.async(work: "test")
    }
    print("replyAsync (remote): \(replyAsync)")
    // CHECK: replyAsync (remote): nil
    print("=== -------------------------------------------------------")

    let replyAsyncThrows = try await remoteW.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrows(work: "test")
    }
    print("replyAsyncThrows (remote): \(replyAsyncThrows)")
    // CHECK: replyAsyncThrows (remote): nil
  }
  print("=== -------------------------------------------------------")

  // ==== ----------------------------------------------------------------------

  do {
    let replyDistSubmit = try await localW.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.submit_witness_sync(work: "local-test")
    }
    print("replyDistSubmit (local): \(replyDistSubmit ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyDistSubmit (local): submit_witness_sync(work:): local-test
    print("=== -------------------------------------------------------")

    let replySyncLocal = await localW.whenLocal { __secretlyKnownToBeLocal in
      __secretlyKnownToBeLocal.sync(work: "local-test")
    }
    print("replySyncLocal (local): \(replySyncLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replySyncLocal (local): sync(work:): local-test
    print("=== -------------------------------------------------------")

    let replySyncThrowsLocal = try await localW.whenLocal { __secretlyKnownToBeLocal in
      try __secretlyKnownToBeLocal.syncThrows(work: "local-test")
    }
    print("replySyncThrowsLocal (local): \(replySyncThrowsLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replySyncThrowsLocal (local): syncThrows(work:): local-test
    print("=== -------------------------------------------------------")

    let replyAsyncLocal = await localW.whenLocal { __secretlyKnownToBeLocal in
      await __secretlyKnownToBeLocal.async(work: "local-test")
    }
    print("replyAsyncLocal (local): \(replyAsyncLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyAsyncLocal (local): async(work:): local-test
    print("=== -------------------------------------------------------")

    let replyAsyncThrowsLocal = try await localW.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrows(work: "local-test")
    }
    print("replyAsyncThrowsLocal (local): \(replyAsyncThrowsLocal ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyAsyncThrowsLocal (local): asyncThrows(work:): local-test
    print("=== -------------------------------------------------------")
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
