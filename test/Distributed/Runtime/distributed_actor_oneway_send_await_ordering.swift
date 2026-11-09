// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature SendCalls -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: distributed
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// The order of
//
//   try send worker.enable(i)
//   try await worker.refresh(i)
//
// is preserved, as seen (1) when each call enters the actor system and (2) when
// each call runs on the remote actor, for a caller which is an actor and for
// one which is a nonisolated async function, and for an actor system whose
// 'remoteCall*' functions are 'nonisolated(nonsending)' as well as for one whose
// are plain 'async'.
//
// This holds because 'send' of a '@oneway' function enters 'remoteCallOneway'
// inline, from the caller, before the next statement runs. The test transport
// is a single in-order channel for both kinds of calls, so any reordering is
// caused by how the calls reach the actor system. A system that hands its
// oneway calls to the transport asynchronously (with 'Task {}' for example)
// does not get this guarantee, the test systems use 'Task.immediate'.

@_spi(OnewaySend) import Distributed
import Synchronization
import FakeDistributedActorSystems

let iterations = 500

struct Box<T>: @unchecked Sendable { var value: T }

/// A single in-order channel that executes the calls one after the other
actor Channel {
  struct Item {
    var run: @Sendable () async throws -> Void
    var reply: CheckedContinuation<Void, Error>?
  }
  var outbox: [Item] = []
  var isDelivering = false

  func submit(_ run: @escaping @Sendable () async throws -> Void) {
    enqueue(Item(run: run, reply: nil))
  }

  func submitAndWait(_ run: @escaping @Sendable () async throws -> Void) async throws {
    try await withCheckedThrowingContinuation { c in
      enqueue(Item(run: run, reply: c))
    }
  }

  func enqueue(_ item: Item) {
    outbox.append(item)
    if !isDelivering {
      isDelivering = true
      Task { await self.deliverAll() }
    }
  }

  func deliverAll() async {
    while !outbox.isEmpty {
      let item = outbox.removeFirst()
      do {
        try await item.run()
        item.reply?.resume()
      } catch {
        item.reply?.resume(throwing: error)
      }
    }
    isDelivering = false
  }
}

/// What both systems share. The systems record the entry of each call
/// themselves, as the first thing they do, because where they run is the point
final class Core: @unchecked Sendable {
  let inner = FakeRoundtripActorSystem()
  let channel = Channel()
  let entries = Mutex<[String]>([])
  let hosts = Mutex<[ActorAddress: Box<any DistributedActor>]>([:])

  func entry(_ what: String) {
    entries.withLock { $0.append(what) }
  }

  func host(of id: ActorAddress) -> any DistributedActor {
    hosts.withLock { $0[id]! }.value
  }
}

func makeRun<S: DistributedActorSystem<Codable>>(
  system: S, host: any DistributedActor, target: RemoteCallTarget,
  decoder: FakeInvocationDecoder
) -> @Sendable () async throws -> Void
    where S.InvocationDecoder == FakeInvocationDecoder,
          S.ResultHandler == FakeRoundtripResultHandler {
  let box = Box(value: (system, host, FakeTargetEnvelope(target), decoder))
  return {
    let (system, host, envelope, decoder) = box.value
    func doIt<A: DistributedActor>(active: A) async throws {
      var decoder = decoder
      try await system.executeDistributedTarget(
        on: active, target: fakeRecipientTarget(envelope),
        invocationDecoder: &decoder,
        handler: FakeRoundtripResultHandler { _ in } onError: { _ in })
    }
    try await _openExistential(host, do: doIt)
  }
}

func describe(_ target: RemoteCallTarget) -> String {
  target.identifier.contains("enable") ? "send" : "await"
}

// ==== -----------------------------------------------------------------------
// MARK: System with 'nonisolated(nonsending)' remote calls

final class NonsendingSystem: _DistributedOnewayActorSystem, @unchecked Sendable {
  typealias ActorID = ActorAddress
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeRoundtripResultHandler
  let core = Core()

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor { nil }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor { core.inner.assignID(actorType) }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    core.hosts.withLock { $0[actor.id] = Box(value: actor) }
  }
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder {
    core.inner.makeInvocationEncoder()
  }

  nonisolated(nonsending)
  func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type, returning returnType: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error,
            Res: SerializationRequirement {
    fatalError("unused")
  }

  nonisolated(nonsending)
  func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder, throwing errorType: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    core.entry("\(describe(target)) \(target.identifier)")
    let run = makeRun(
      system: self, host: core.host(of: actor.id), target: target,
      decoder: invocation.makeDecoder())
    try await core.channel.submitAndWait(run)
  }

  func remoteCallOneway<Act>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder
  ) throws where Act: DistributedActor, Act.ID == ActorID {
    core.entry("\(describe(target)) \(target.identifier)")
    let run = makeRun(
      system: self, host: core.host(of: actor.id), target: target,
      decoder: invocation.makeDecoder())
    let channel = core.channel
    Task.immediate { await channel.submit(run) }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: System with plain 'async' remote calls

final class PlainSystem: _DistributedOnewayActorSystem, @unchecked Sendable {
  typealias ActorID = ActorAddress
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeRoundtripResultHandler
  let core = Core()

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor { nil }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor { core.inner.assignID(actorType) }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    core.hosts.withLock { $0[actor.id] = Box(value: actor) }
  }
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder {
    core.inner.makeInvocationEncoder()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type, returning returnType: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error,
            Res: SerializationRequirement {
    fatalError("unused")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder, throwing errorType: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    core.entry("\(describe(target)) \(target.identifier)")
    let run = makeRun(
      system: self, host: core.host(of: actor.id), target: target,
      decoder: invocation.makeDecoder())
    try await core.channel.submitAndWait(run)
  }

  func remoteCallOneway<Act>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder
  ) throws where Act: DistributedActor, Act.ID == ActorID {
    core.entry("\(describe(target)) \(target.identifier)")
    let run = makeRun(
      system: self, host: core.host(of: actor.id), target: target,
      decoder: invocation.makeDecoder())
    let channel = core.channel
    Task.immediate { await channel.submit(run) }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Workers

distributed actor NonsendingWorker {
  typealias ActorSystem = NonsendingSystem
  var log: [String] = []
  @oneway distributed func enable(_ i: Int) { log.append("enable \(i)") }
  distributed func refresh(_ i: Int) { log.append("refresh \(i)") }
  distributed func snapshot() -> [String] { log }
}

distributed actor PlainWorker {
  typealias ActorSystem = PlainSystem
  var log: [String] = []
  @oneway distributed func enable(_ i: Int) { log.append("enable \(i)") }
  distributed func refresh(_ i: Int) { log.append("refresh \(i)") }
  distributed func snapshot() -> [String] { log }
}

// ==== -----------------------------------------------------------------------
// MARK: Callers

actor NonsendingActorCaller {
  func run(_ w: NonsendingWorker) async throws {
    for i in 0 ..< iterations {
      try send w.enable(i)
      try await w.refresh(i)
    }
  }
}

actor PlainActorCaller {
  func run(_ w: PlainWorker) async throws {
    for i in 0 ..< iterations {
      try send w.enable(i)
      try await w.refresh(i)
    }
  }
}

func nonisolatedCaller(_ w: NonsendingWorker) async throws {
  for i in 0 ..< iterations {
    try send w.enable(i)
    try await w.refresh(i)
  }
}

func nonisolatedCaller(_ w: PlainWorker) async throws {
  for i in 0 ..< iterations {
    try send w.enable(i)
    try await w.refresh(i)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Analysis

/// Counts the iterations in which `first i` is not before `second i`
func reordered(_ log: [String], _ first: String, _ second: String) -> Int {
  var count = 0
  var firstIndex: [Int: Int] = [:]
  var secondIndex: [Int: Int] = [:]
  for (index, line) in log.enumerated() {
    let parts = line.split(separator: " ")
    guard let n = Int(parts.last ?? "") else { continue }
    if parts.first == Substring(first) { firstIndex[n] = index }
    if parts.first == Substring(second) { secondIndex[n] = index }
  }
  for n in 0 ..< iterations {
    if let f = firstIndex[n], let s = secondIndex[n], f > s { count += 1 }
  }
  return count
}

func report(_ system: String, _ caller: String, remoteLog: [String], entryLog: [String]) {
  print("[swift] RESULT system=\(system) caller=\(caller) iterations=\(iterations) " +
        "entry-reordered=\(reordered(entryLog, "send", "await")) " +
        "remote-reordered=\(reordered(remoteLog, "enable", "refresh")) " +
        "remote-count=\(remoteLog.count)")
}

func waitForRemote(_ count: Int, _ snapshot: () async throws -> [String]) async rethrows -> [String] {
  for _ in 0 ..< 1_000_000 {
    let log = try await snapshot()
    if log.count >= count { return log }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'send' calls to run")
}

@main struct Main {
  static func main() async throws {
    // Entries are logged as "send <identifier>", the index is not in the
    // identifier, so tag the iteration through the worker logs only. For the
    // entry log, pair the k-th 'send' with the k-th 'await'.
    do {
      let system = NonsendingSystem()
      let host = NonsendingWorker(actorSystem: system)
      let remote = try NonsendingWorker.resolve(id: host.id, using: system)
      try await NonsendingActorCaller().run(remote)
      let log = try await waitForRemote(2 * iterations) { try await host.snapshot() }
      report("nonsending", "actor", remoteLog: log, entryLog: numbered(system.core.entries.withLock { $0 }))
    }
    do {
      let system = NonsendingSystem()
      let host = NonsendingWorker(actorSystem: system)
      let remote = try NonsendingWorker.resolve(id: host.id, using: system)
      try await nonisolatedCaller(remote)
      let log = try await waitForRemote(2 * iterations) { try await host.snapshot() }
      report("nonsending", "nonisolated", remoteLog: log, entryLog: numbered(system.core.entries.withLock { $0 }))
    }
    do {
      let system = PlainSystem()
      let host = PlainWorker(actorSystem: system)
      let remote = try PlainWorker.resolve(id: host.id, using: system)
      try await PlainActorCaller().run(remote)
      let log = try await waitForRemote(2 * iterations) { try await host.snapshot() }
      report("plain", "actor", remoteLog: log, entryLog: numbered(system.core.entries.withLock { $0 }))
    }
    do {
      let system = PlainSystem()
      let host = PlainWorker(actorSystem: system)
      let remote = try PlainWorker.resolve(id: host.id, using: system)
      try await nonisolatedCaller(remote)
      let log = try await waitForRemote(2 * iterations) { try await host.snapshot() }
      report("plain", "nonisolated", remoteLog: log, entryLog: numbered(system.core.entries.withLock { $0 }))
    }
    // CHECK: [swift] RESULT system=nonsending caller=actor iterations=500 entry-reordered=0 remote-reordered=0 remote-count=1000
    // CHECK: [swift] RESULT system=nonsending caller=nonisolated iterations=500 entry-reordered=0 remote-reordered=0 remote-count=1000
    // CHECK: [swift] RESULT system=plain caller=actor iterations=500 entry-reordered=0 remote-reordered=0 remote-count=1000
    // CHECK: [swift] RESULT system=plain caller=nonisolated iterations=500 entry-reordered=0 remote-reordered=0 remote-count=1000
  }
}

/// "send <id>" / "await <id>" entries become "send <k>" / "await <k>", where k
/// is the number of earlier entries of the same kind, i.e. the iteration
func numbered(_ entries: [String]) -> [String] {
  var counts: [String: Int] = [:]
  return entries.map { line in
    let kind = String(line.split(separator: " ").first!)
    let k = counts[kind, default: 0]
    counts[kind] = k + 1
    return "\(kind) \(k)"
  }
}
