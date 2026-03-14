// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library -plugin-path %swift-plugin-dir -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

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

@available(SwiftStdlib 6.0, *)
protocol PlainWorker {
  associatedtype WorkItem: Sendable & Codable
  associatedtype WorkResult: Sendable & Codable

  /// Can be witnessed by a distributed actor method:
  func asyncThrows(work: WorkItem) async throws -> WorkResult
}

@Resolvable
@available(SwiftStdlib 6.0, *)
protocol DistributedWorker<WorkItem, WorkResult>: DistributedActor where ActorSystem == DefaultDistributedActorSystem {
  associatedtype WorkItem: Sendable & Codable
  associatedtype WorkResult: Sendable & Codable

  distributed func dist_sync(work: WorkItem) -> WorkResult
  distributed func dist_async(work: WorkItem) async -> WorkResult
  distributed func dist_syncThrows(work: WorkItem) throws -> WorkResult
  distributed func dist_asyncThrows(work: WorkItem) async throws -> WorkResult

  // non distributed requirements can be witnessed with _normal_ functions
  func sync(work: WorkItem) -> WorkResult
  func async(work: WorkItem) async -> WorkResult
  func syncThrows(work: WorkItem) throws -> WorkResult
  func asyncThrows(work: WorkItem) async throws -> WorkResult

  func asyncThrowsReq_witnessDistributed_sync(work: WorkItem) async throws -> WorkResult
  func asyncThrowsReq_witnessDistributed_async(work: WorkItem) async throws -> WorkResult
  func asyncThrowsReq_witnessDistributed_syncThrows(work: WorkItem) async throws -> WorkResult
  func asyncThrowsReq_witnessDistributed_asyncThrows(work: WorkItem) async throws -> WorkResult
}

@available(SwiftStdlib 6.0, *)
distributed actor ThePlainWorker: PlainWorker {
  typealias ActorSystem = DefaultDistributedActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func asyncThrows(work: WorkItem) async throws -> WorkResult {
    return "\(#function): \(work)"
  }
}

@available(SwiftStdlib 6.0, *)
distributed actor TheWorker: DistributedWorker {
  typealias ActorSystem = DefaultDistributedActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func dist_sync(work: WorkItem) -> WorkResult {
    "\(#function): \(work)"
  }
  distributed func dist_async(work: WorkItem) async -> WorkResult {
    "\(#function): \(work)"
  }
  distributed func dist_syncThrows(work: WorkItem) throws -> WorkResult {
    "\(#function): \(work)"
  }
  distributed func dist_asyncThrows(work: WorkItem) async throws -> WorkResult {
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

  distributed func asyncThrowsReq_witnessDistributed_sync(work: WorkItem) -> WorkResult {
    return "\(#function): \(work)"
  }
  distributed func asyncThrowsReq_witnessDistributed_async(work: WorkItem) async -> WorkResult {
    return "\(#function): \(work)"
  }
  distributed func asyncThrowsReq_witnessDistributed_syncThrows(work: WorkItem) throws -> WorkResult {
    return "\(#function): \(work)"
  }
  distributed func asyncThrowsReq_witnessDistributed_asyncThrows(work: WorkItem) async throws -> WorkResult {
    return "\(#function): \(work)"
  }

}

@available(SwiftStdlib 6.0, *)
func test_generic(system: DefaultDistributedActorSystem) async throws {
  let localW = TheWorker(actorSystem: system)
  let remoteW = try! TheWorker.resolve(id: localW.id, using: system)
  precondition(__isRemoteActor(remoteW))

  // direct calls work ok:
  do {
    let reply = try await remoteW.dist_sync(work: "Direct")
    print("replySync direct (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_sync(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    // CHECK: replySync direct (remote): dist_sync(work:): Direct
  }
  print("==== ----------------------------------------------------------------")

  do {
    let reply = try await remoteW.dist_async(work: "Direct")
    print("replyAsync direct (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_async(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    // CHECK: replyAsync direct (remote): dist_async(work:): Direct
  }
  print("==== ----------------------------------------------------------------")

  do {
    let reply = try await remoteW.dist_syncThrows(work: "Direct")
    print("replyThrows direct (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_syncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
    // CHECK: replyThrows direct (remote): dist_syncThrows(work:): Direct
  }
  print("==== ----------------------------------------------------------------")

  do {
    let reply = try await remoteW.dist_asyncThrows(work: "Direct")
    print("replyAsyncThrows direct (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_asyncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
    // CHECK: replyAsyncThrows direct (remote): dist_asyncThrows(work:): Direct
  }
  print("==== ----------------------------------------------------------------")


  func call_dist_sync<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.dist_sync(work: "Hello")
  }
  do {
    let reply = try await call_dist_sync(w: remoteW)
    print("reply (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_sync(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    // CHECK: << remoteCall return: dist_sync(work:): Hello
    // CHECK: reply (remote): dist_sync(work:): Hello

    let replyLocal = try await call_dist_sync(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): dist_sync(work:): Hello
  }
  print("==== ----------------------------------------------------------------")

  func call_dist_async<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.dist_async(work: "Hello")
  }
  do {
    let reply = try await call_dist_async(w: remoteW)
    print("reply (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_async(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    // CHECK: << remoteCall return: dist_async(work:): Hello
    // CHECK: reply (remote): dist_async(work:): Hello

    let replyLocal = try await call_dist_async(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): dist_async(work:): Hello
  }
  print("==== ----------------------------------------------------------------")

  func call_dist_throws<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.dist_syncThrows(work: "Hello")
  }
  do {
    let reply = try await call_dist_throws(w: remoteW)
    print("reply (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_syncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
    // CHECK: << remoteCall return: dist_syncThrows(work:): Hello
    // CHECK: reply (remote): dist_syncThrows(work:): Hello

    let replyLocal = try await call_dist_throws(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): dist_syncThrows(work:): Hello
  }
  print("==== ----------------------------------------------------------------")

  func call_dist_asyncThrows<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.dist_asyncThrows(work: "Hello")
  }
  do {
    let reply = try await call_dist_asyncThrows(w: remoteW)
    print("reply (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.TheWorker, target:main.TheWorker.dist_asyncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
    // CHECK: << remoteCall return: dist_asyncThrows(work:): Hello
    // CHECK: reply (remote): dist_asyncThrows(work:): Hello

    let replyLocal = try await call_dist_asyncThrows(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): dist_asyncThrows(work:): Hello
  }
  print("==== ----------------------------------------------------------------")
  print("=====================================================================")

  // These tests verify that we can call these methods if we "peel off" distributed
  // isolation, and they'll end up invoking the distributed witnesses. Though the
  // distributedness of those witnesses never actually is used remotely, but at
  // least check we invoke the right methods.

  @available(SwiftStdlib 6.0, *)
  func call_requirement_witnessedByDistributed_sync<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrowsReq_witnessDistributed_sync(work: "Hello")
    }!
  }
  do {
    let replyLocal = try await call_requirement_witnessedByDistributed_sync(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): asyncThrowsReq_witnessDistributed_sync(work:): Hello
  }
  print("==== ----------------------------------------------------------------")

  @available(SwiftStdlib 6.0, *)
  func call_requirement_witnessedByDistributed_async<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrowsReq_witnessDistributed_async(work: "Hello")
    }!
  }
  do {
    let replyLocal = try await call_requirement_witnessedByDistributed_async(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): asyncThrowsReq_witnessDistributed_async(work:): Hello
  }
  print("==== ----------------------------------------------------------------")

  @available(SwiftStdlib 6.0, *)
  func call_requirement_witnessedByDistributed_syncThrows<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrowsReq_witnessDistributed_syncThrows(work: "Hello")
    }!
  }
  do {
    let replyLocal = try await call_requirement_witnessedByDistributed_syncThrows(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): asyncThrowsReq_witnessDistributed_syncThrows(work:): Hello
  }
  print("==== ----------------------------------------------------------------")

  @available(SwiftStdlib 6.0, *)
  func call_requirement_witnessedByDistributed_asyncThrows<W: DistributedWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.whenLocal { __secretlyKnownToBeLocal in
      try await __secretlyKnownToBeLocal.asyncThrowsReq_witnessDistributed_asyncThrows(work: "Hello")
    }!
  }
  do {
    let replyLocal = try await call_requirement_witnessedByDistributed_asyncThrows(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): asyncThrowsReq_witnessDistributed_asyncThrows(work:): Hello
  }
  print("==== ----------------------------------------------------------------")
}

@available(SwiftStdlib 6.0, *)
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
      try await __secretlyKnownToBeLocal.dist_sync(work: "local-test")
    }
    print("replyDistSync (local): \(replyDistSubmit ?? "nil")")
    // CHECK-NOT: >> remoteCall
    // CHECK: replyDistSync (local): dist_sync(work:): local-test

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

@available(SwiftStdlib 6.0, *)
func test_generic_plain(system: DefaultDistributedActorSystem) async throws {
  let localW = ThePlainWorker(actorSystem: system)
  let remoteW = try! ThePlainWorker.resolve(id: localW.id, using: system)
  precondition(__isRemoteActor(remoteW))

  // direct calls work ok:
  do {
    let reply = try await remoteW.asyncThrows(work: "Direct")
    print("replySync direct (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.ThePlainWorker, target:main.ThePlainWorker.asyncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Direct"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
    // CHECK: replySync direct (remote): asyncThrows(work:): Direct
  }
  print("==== ----------------------------------------------------------------")

  @available(SwiftStdlib 6.0, *)
  func call_plainWorker<W: PlainWorker>(w: W) async throws -> String where W.WorkItem == String, W.WorkResult == String {
    try await w.asyncThrows(work: "Hello")
  }
  do {
    let reply = try await call_plainWorker(w: remoteW)
    print("reply (remote): \(reply)")
    // CHECK: >> remoteCall: on:main.ThePlainWorker, target:main.ThePlainWorker.asyncThrows(work:), invocation:FakeInvocationEncoder(genericSubs: [], arguments: ["Hello"], returnType: Optional(Swift.String), errorType: Optional(Swift.Error)), throwing:Swift.Error, returning:Swift.String
    // CHECK: << remoteCall return: asyncThrows(work:): Hello
    // CHECK: reply (remote): asyncThrows(work:): Hello

    let replyLocal = try await call_plainWorker(w: localW)
    print("reply (local): \(replyLocal)")
    // CHECK-NOT: >> remoteCall
    // CHECK: reply (local): asyncThrows(work:): Hello
  }
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    let system = DefaultDistributedActorSystem()
    print("===================================================================")
    try! await test_generic(system: system)
    print("===================================================================")
    try! await test_whenLocal(system: system)
    print("===================================================================")
    try! await test_generic_plain(system: system)
  }
}
