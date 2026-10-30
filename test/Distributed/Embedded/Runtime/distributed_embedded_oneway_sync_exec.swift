// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -module-name main -plugin-path %swift-plugin-dir %s %S/Inputs/EmbeddedFakeActorSystem.swift -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// In Embedded Swift a '@oneway' distributed func with a synchronous body gets a
// synchronous distributed thunk, so 'try send' works from synchronous code:
// - a local target is enqueued on the actor (a discarding task that copies the
//   caller's task locals and starts on the actor's executor)
// - a remote target goes through the synchronous 'remoteCallOneway' of the
//   '_DistributedOnewayActorSystem'
// - the receive dispatcher enqueues a 'oneway' remote call and sends no
//   reply, but makes any other call of it, which waits for a reply (e.g. one
//   of a client compiled against a version of the func which was not
//   '@oneway', or an 'await'), on the actor and replies to it
// - a sent and an awaited remote call use the same identifier, only
//   'isOnewayRemoteCall' tells them apart

import _Concurrency
@_spi(OnewaySend) import Distributed

// Polls until 'done' returns true, and fails instead of hanging forever if the
// fire-and-forget calls never run
func waitUntil(_ done: () async throws -> Bool) async rethrows {
  for _ in 0 ..< 1_000_000 {
    if try await done() {
      return
    }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'send' calls to run")
}

enum Probe {
  @TaskLocal static var tag: Int = 0
}

// ==== -----------------------------------------------------------------------
// MARK: An actor system which implements 'remoteCallOneway'

struct ProbeResultHandler: DistributedTargetInvocationResultHandler {
  func onReturnVoid() async throws { print("[swift] onReturnVoid") }
  func onThrow(error: any Error) async throws { print("[swift] onThrow") }
}
extension ProbeResultHandler {
  func onReturn<Success: EmbeddedSerializationRequirement>(value: Success) async throws {
    print("[swift] onReturn")
  }
}

final class ProbeActorSystem: _DistributedOnewayActorSystem, @unchecked Sendable {
  typealias ActorID = EmbeddedFakeActorID
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  typealias InvocationEncoder = EmbeddedFakeInvocationEncoder
  typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  typealias ResultHandler = ProbeResultHandler

  typealias LocalDispatch =
    (borrowing RemoteCallTarget, inout InvocationDecoder, ResultHandler) async throws -> Void

  var active: [ActorID: LocalDispatch] = [:]
  var nextID: UInt64 = 1

  // The last message sent by 'remoteCallOneway'
  var lastIdentifier: [UInt8] = []
  var lastArguments: [UInt8] = []

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
      // Only calls with a oneway flag print, so that the tests which match the
      // output line by line don't see the extra line
      if target.isOnewayRemoteCall || target.isOnewayTarget {
        print("[swift] executeDistributedTarget: oneway: \(target.isOnewayRemoteCall) target: \(target.isOnewayTarget)")
      }
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
    // An awaited call of a '@oneway' func uses the same identifier as the
    // sent one, and is not a 'oneway' remote call
    let identifierBytes = unsafe target.identifier.withUnsafeBytes { unsafe [UInt8]($0) }
    print("[swift] remoteCallVoid reached, oneway: \(target.isOnewayRemoteCall), same identifier as the last send: \(identifierBytes == lastIdentifier)")
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes
    let receivedTarget = RemoteCallTarget(identifierBytes.span.bytes)
    var decoder = InvocationDecoder(buffer: requestBuffer)
    try await dispatch(receivedTarget, &decoder, ResultHandler())
  }

  // The synchronous send: hand the serialized arguments to the "peer" and
  // return without waiting for any reply
  func remoteCallOneway<Act>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder
  ) throws
      where Act: DistributedActor, Act.ActorSystem == ProbeActorSystem {
    print("[swift] remoteCallOneway reached, oneway: \(target.isOnewayRemoteCall), onewayTarget: \(target.isOnewayTarget)")
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }

    // The network: send the request to the callee
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes
    // 'RemoteCallTarget' is non-escapable, so send its identifier bytes and
    // re-create the target on the receiving side
    let identifierBytes = unsafe target.identifier.withUnsafeBytes { unsafe [UInt8]($0) }
    // Whether the caller waits for a reply is a property of the call, which
    // the message carries to the recipient
    let isOneway = target.isOnewayRemoteCall
    let isOnewayTarget = target.isOnewayTarget
    lastIdentifier = identifierBytes
    lastArguments = invocation.buffer.argBytes

    // The receiving side runs in its own task-local context
    Probe.$tag.withValue(42) {
      Task.immediate {
        var receivedTarget = RemoteCallTarget(identifierBytes.span.bytes)
        receivedTarget.isOnewayRemoteCall = isOneway
        receivedTarget.isOnewayTarget = isOnewayTarget
        var decoder = InvocationDecoder(buffer: requestBuffer)
        let handler = ResultHandler()
        try await dispatch(receivedTarget, &decoder, handler)
        print("[swift] dispatch returned")
      }
    }
  }

  // Delivers the last message sent by 'remoteCallOneway' again, as a call which is
  // not a 'oneway' remote call and whose caller waits for the reply, like the
  // 'remoteCallVoid' of a client compiled against a version of the target
  // which was not '@oneway'
  func deliverLastAsAwaitedCall<Act>(to actor: Act) async throws
      where Act: DistributedActor, Act.ActorSystem == ProbeActorSystem {
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = lastArguments
    let identifierBytes = lastIdentifier
    let receivedTarget = RemoteCallTarget(identifierBytes.span.bytes)
    print("[swift] awaited call, oneway: \(receivedTarget.isOnewayRemoteCall)")
    var decoder = InvocationDecoder(buffer: requestBuffer)
    try await dispatch(receivedTarget, &decoder, ResultHandler())
    print("[swift] awaited call replied")
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Actors

distributed actor Stepper {
  typealias ActorSystem = EmbeddedFakeRoundtripActorSystem

  var seen: [Int] = []

  @oneway distributed func step(_ n: Int) {
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

  @oneway distributed func step(_ n: Int) {
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
      try send s.step(i)
    }
  }
}

func sendRemote(_ s: Stepper) throws {
  try Probe.$tag.withValue(8) {
    for i in 11 ... 13 {
      try send s.step(i)
    }
  }
}

func sendProbe(_ s: ProbeStepper) throws {
  for i in 21 ... 23 {
    try send s.step(i)
  }
}

func sendProbeOnce(_ s: ProbeStepper) throws {
  try send s.step(31)
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
  // 'EmbeddedFakeRoundtripActorSystem' runs the calls on the receiving side
  // from a 'Task.immediate', which copies the caller's task locals
  try sendRemote(remote)
  try await waitUntil { try await local.count() >= 3 }
  print("[swift] remote delivered: \(try await local.order())")
}
// CHECK-LABEL: [swift] test_remote_default
// CHECK-DAG: [swift] remoteCallOneway reached, oneway: true, onewayTarget: true
// The recipient sees the flags the sender set
// CHECK-DAG: [swift] executeDistributedTarget: oneway: true target: true
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
// CHECK: [swift] remoteCallOneway reached, oneway: true, onewayTarget: true
// The dispatcher arm enqueues the call and reports no result to the handler
// CHECK-NOT: [swift] onReturnVoid
// CHECK-NOT: [swift] onThrow
// CHECK-DAG: [swift] executeDistributedTarget: oneway: true target: true
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

func test_remote_awaited_call_replies() async throws {
  print("[swift] test_remote_awaited_call_replies")
  let system = ProbeActorSystem()
  let local = ProbeStepper(actorSystem: system)
  let remote = try ProbeStepper.resolve(id: local.id, using: system)
  // A 'oneway' remote call is not replied to
  try sendProbeOnce(remote)
  try await waitUntil { try await local.count() >= 1 }
  print("[swift] oneway delivered: \(try await local.order())")
  // The same message, as a call which waits for a reply, is made on the
  // actor before it is replied to, and the caller resumes
  try await system.deliverLastAsAwaitedCall(to: local)
  print("[swift] awaited delivered: \(try await local.order())")
  // An 'await' of the remote actor makes that call through 'remoteCallVoid',
  // with the same identifier as the 'send'
  try await remote.step(32)
  print("[swift] awaited remote call returned: \(try await local.order())")
}
// CHECK-LABEL: [swift] test_remote_awaited_call_replies
// CHECK: [swift] remoteCallOneway reached, oneway: true, onewayTarget: true
// CHECK-NOT: [swift] onReturnVoid
// CHECK-NOT: [swift] onThrow
// CHECK: [swift] oneway delivered: 31
// CHECK-NEXT: [swift] awaited call, oneway: false
// CHECK-NEXT: [swift] ProbeStepper.step(31) tag: 0
// CHECK-NEXT: [swift] onReturnVoid
// CHECK-NEXT: [swift] awaited call replied
// CHECK-NEXT: [swift] awaited delivered: 31 31
// CHECK-NEXT: [swift] remoteCallVoid reached, oneway: false, same identifier as the last send: true
// CHECK-NEXT: [swift] ProbeStepper.step(32) tag: 0
// CHECK-NEXT: [swift] onReturnVoid
// CHECK-NEXT: [swift] awaited remote call returned: 31 31 32

@main struct Main {
  static func main() async {
    do {
      try await test_local()
      try await test_remote_default()
      try await test_remote_sync_and_dispatcher()
      try await test_remote_awaited_call_replies()
    } catch {
      print("[swift] threw")
    }
    print("[swift] done")
  }
}
// CHECK: [swift] done
