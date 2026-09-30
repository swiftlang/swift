// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -module-name main -plugin-path %swift-plugin-dir %s %S/Inputs/EmbeddedFakeActorSystem.swift -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait

// In Embedded Swift a 'oneway' distributed func with a synchronous body gets a
// synchronous distributed thunk, so 'try nowait' works from synchronous code:
// - a local target is enqueued on the actor (a discarding task that copies the
//   caller's task locals and starts on the actor's executor)
// - a remote target goes through the synchronous 'remoteCallVoidOneway', or
//   its 'Task.immediate' default implementation if the system has none
// - the receive dispatcher enqueues the call and sends no reply

import _Concurrency
import Distributed

// Polls until 'done' returns true, and fails instead of hanging forever if the
// fire-and-forget calls never run
func waitUntil(_ done: () async throws -> Bool) async rethrows {
  for _ in 0 ..< 1_000_000 {
    if try await done() {
      return
    }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'nowait' calls to run")
}

enum Probe {
  @TaskLocal static var tag: Int = 0
}

// ==== -----------------------------------------------------------------------
// MARK: An actor system which implements 'remoteCallVoidOneway'

struct ProbeResultHandler: DistributedTargetInvocationResultHandler {
  func onReturnVoid() async throws { print("[swift] onReturnVoid") }
  func onThrow(error: any Error) async throws { print("[swift] onThrow") }
}
extension ProbeResultHandler {
  func onReturn<Success: EmbeddedSerializationRequirement>(value: Success) async throws {
    print("[swift] onReturn")
  }
}

final class ProbeActorSystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = EmbeddedFakeActorID
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  typealias InvocationEncoder = EmbeddedFakeInvocationEncoder
  typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  typealias ResultHandler = ProbeResultHandler

  typealias LocalDispatch =
    (borrowing RemoteCallTarget, inout InvocationDecoder, ResultHandler) async throws -> Void

  var active: [ActorID: LocalDispatch] = [:]
  var nextID: UInt64 = 1

  init() {}

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == ProbeActorSystem {
    return nil // always remote
  }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == ProbeActorSystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == ProbeActorSystem {
    active[actor.id] = { [self] target, decoder, handler in
      try await self.executeDistributedTarget(
          on: actor, target: target, invocationDecoder: &decoder, handler: handler)
    }
  }
  func resignID(_ id: ActorID) {
    active.removeValue(forKey: id)
  }

  func makeInvocationEncoder() -> InvocationEncoder { .init(buffer: CallBuffer()) }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor,
            Act.ID == ActorID,
            Err: Error,
            Res: EmbeddedSerializationRequirement {
    fatalError("[swift] unexpected remoteCall")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    fatalError("[swift] unexpected remoteCallVoid")
  }

  // The synchronous send: hand the serialized arguments to the "peer" and
  // return without waiting for any reply
  func remoteCallVoidOneway<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) throws
      where Act: DistributedActor, Act.ActorSystem == ProbeActorSystem, Err: Error {
    print("[swift] remoteCallVoidOneway reached, oneway: \(target.isOnewayRemoteCall)")
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }

    // The network: send the request to the callee
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes
    // 'RemoteCallTarget' is non-escapable, so send its identifier bytes and
    // re-create the target on the receiving side
    let identifierBytes = unsafe target.identifier.withUnsafeBytes { unsafe [UInt8]($0) }

    // The receiving side runs in its own task-local context
    Probe.$tag.withValue(42) {
      Task.immediate {
        let receivedTarget = RemoteCallTarget(identifierBytes.span.bytes)
        var decoder = InvocationDecoder(buffer: requestBuffer)
        let handler = ResultHandler()
        try await dispatch(receivedTarget, &decoder, handler)
        print("[swift] dispatch returned")
      }
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Actors

distributed actor Stepper {
  typealias ActorSystem = EmbeddedFakeRoundtripActorSystem

  var seen: [Int] = []

  distributed func step(_ n: Int) oneway {
    seen.append(n)
    print("[swift] Stepper.step(\(n)) tag: \(Probe.tag)")
  }

  distributed func count() -> Int { seen.count }

  // The order in which the oneway calls ran, e.g. "11 12 13"
  distributed func order() -> String {
    var out = ""
    for n in seen {
      if !out.isEmpty { out += " " }
      out += "\(n)"
    }
    return out
  }
}

distributed actor ProbeStepper {
  typealias ActorSystem = ProbeActorSystem

  var seen: [Int] = []

  distributed func step(_ n: Int) oneway {
    seen.append(n)
    print("[swift] ProbeStepper.step(\(n)) tag: \(Probe.tag)")
  }

  distributed func count() -> Int { seen.count }

  // The order in which the oneway calls ran, e.g. "11 12 13"
  distributed func order() -> String {
    var out = ""
    for n in seen {
      if !out.isEmpty { out += " " }
      out += "\(n)"
    }
    return out
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Synchronous senders

func sendLocal(_ s: Stepper) throws {
  try Probe.$tag.withValue(7) {
    for i in 1 ... 3 {
      try nowait s.step(i)
    }
  }
}

func sendRemote(_ s: Stepper) throws {
  try Probe.$tag.withValue(8) {
    for i in 11 ... 13 {
      try nowait s.step(i)
    }
  }
}

func sendProbe(_ s: ProbeStepper) throws {
  for i in 21 ... 23 {
    try nowait s.step(i)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

func test_local() async throws {
  print("[swift] test_local")
  let system = EmbeddedFakeRoundtripActorSystem()
  let local = Stepper(actorSystem: system)
  try sendLocal(local)
  // FIFO: the awaited call is enqueued after the three oneway calls
  let n = try await local.count()
  print("[swift] local count: \(n)")
}
// CHECK-LABEL: [swift] test_local
// CHECK-NEXT: [swift] Stepper.step(1) tag: 7
// CHECK-NEXT: [swift] Stepper.step(2) tag: 7
// CHECK-NEXT: [swift] Stepper.step(3) tag: 7
// CHECK-NEXT: [swift] local count: 3

func test_remote_default() async throws {
  print("[swift] test_remote_default")
  let system = EmbeddedFakeRoundtripActorSystem()
  let local = Stepper(actorSystem: system)
  let remote = try Stepper.resolve(id: local.id, using: system)
  // 'EmbeddedFakeRoundtripActorSystem' has no 'remoteCallVoidOneway', so the
  // default implementation delivers through 'remoteCallVoid'
  try sendRemote(remote)
  try await waitUntil { try await local.count() >= 3 }
  print("[swift] remote delivered: \(try await local.order())")
}
// CHECK-LABEL: [swift] test_remote_default
// CHECK-DAG: [swift] remoteCallVoid reached
// CHECK-DAG: [swift] Stepper.step(11) tag: 8
// CHECK-DAG: [swift] Stepper.step(12) tag: 8
// CHECK-DAG: [swift] Stepper.step(13) tag: 8
// FIFO across the three sends
// CHECK: [swift] remote delivered: 11 12 13

func test_remote_sync_and_dispatcher() async throws {
  print("[swift] test_remote_sync_and_dispatcher")
  let system = ProbeActorSystem()
  let local = ProbeStepper(actorSystem: system)
  let remote = try ProbeStepper.resolve(id: local.id, using: system)
  try sendProbe(remote)
  try await waitUntil { try await local.count() >= 3 }
  print("[swift] probe delivered: \(try await local.order())")
}
// CHECK-LABEL: [swift] test_remote_sync_and_dispatcher
// CHECK: [swift] remoteCallVoidOneway reached, oneway: true
// The dispatcher arm enqueues the call and reports no result to the handler
// CHECK-NOT: [swift] onReturnVoid
// CHECK-NOT: [swift] onThrow
// CHECK-DAG: [swift] dispatch returned
// CHECK-DAG: [swift] ProbeStepper.step(21) tag: 42
// CHECK-DAG: [swift] ProbeStepper.step(22) tag: 42
// CHECK-DAG: [swift] ProbeStepper.step(23) tag: 42
// CHECK-NOT: [swift] onReturnVoid
// CHECK-NOT: [swift] onThrow
// FIFO across the three sends
// CHECK: [swift] probe delivered: 21 22 23
// CHECK-NOT: [swift] onReturnVoid
// CHECK-NOT: [swift] onThrow

@main struct Main {
  static func main() async {
    do {
      try await test_local()
      try await test_remote_default()
      try await test_remote_sync_and_dispatcher()
    } catch {
      print("[swift] threw")
    }
    print("[swift] done")
  }
}
// CHECK: [swift] done
