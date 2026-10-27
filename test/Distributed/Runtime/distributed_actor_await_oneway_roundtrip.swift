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

// An 'await' of a '@oneway' distributed function is only warned about,
// and is a regular request-reply call: it calls the function's await thunk,
// which waits for a local call, and makes a remote call through
// 'remoteCallVoid', not marked as a 'oneway' remote call, so the caller
// waits for the reply and gets the errors that the actor system throws. A
// 'send' calls the synchronous thunk, which makes a remote call through
// 'remoteCallOneway', marked as a 'oneway' remote call. Both mark the target as
// the target of a '@oneway' function ('isOnewayTarget'). The same holds for calls
// through a distributed protocol requirement, an existential and a
// '@Resolvable' stub. ('send' of a distributed
// function which is not '@oneway' is an error, see
// distributed_send_requires_oneway.swift)

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = RecordingSystem

struct TransportError: Error {}

/// Records how the calls are made, and runs them through a
/// 'FakeRoundtripActorSystem'. It resolves every reference as remote
final class RecordingSystem: _DistributedOnewayActorSystem, @unchecked Sendable {
  typealias ActorID = ActorAddress
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeRoundtripResultHandler

  let inner = FakeRoundtripActorSystem()

  /// Makes 'remoteCallVoid' throw instead of making the call
  var failAwaitedCalls = false

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor {
    nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
    inner.assignID(actorType)
  }

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    inner.actorReady(actor)
  }

  func resignID(_ id: ActorID) {
    inner.resignID(id)
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    inner.makeInvocationEncoder()
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type, returning returnType: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error,
            Res: SerializationRequirement {
    try await inner.remoteCall(
      on: actor, target: target, invocation: &invocation,
      throwing: errorType, returning: returnType)
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder, throwing errorType: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    print("[swift] \(#fileID):\(#line) system.remoteCallVoid \(target.identifier) oneway: \(target.isOnewayRemoteCall) target: \(target.isOnewayTarget)")
    if failAwaitedCalls {
      throw TransportError()
    }
    try await inner.remoteCallVoid(
      on: actor, target: target, invocation: &invocation,
      throwing: errorType)
    print("[swift] \(#fileID):\(#line) system.remoteCallVoid returned")
  }

  func remoteCallOneway<Act>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder
  ) throws where Act: DistributedActor, Act.ID == ActorID {
    print("[swift] \(#fileID):\(#line) system.remoteCallOneway \(target.identifier) oneway: \(target.isOnewayRemoteCall) target: \(target.isOnewayTarget)")
    try inner.remoteCallOneway(on: actor, target: target, invocation: &invocation)
  }
}

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

@Resolvable
protocol Teller: DistributedActor where ActorSystem == RecordingSystem {
  @oneway distributed func tell(_ n: Int)
}

distributed actor Worker: Teller {
  var ran: [Int] = []

  @oneway distributed func tell(_ n: Int) {
    ran.append(n)
    print("[swift] \(#fileID):\(#line) Worker.tell(\(n))")
  }

  distributed func count() -> Int { ran.count }
}

func test(_ w: Worker, _ label: String) async throws {
  print("[swift] test \(label)")

  // Waits for the call to have run
  try await w.tell(1)
  print("[swift] awaited tell(1)")

  try send w.tell(2)
  try await waitUntil { try await w.count() >= 2 }
  print("[swift] sent tell(2)")
}

func testThroughRequirement<T: Teller>(_ t: T, _ label: String) async throws {
  print("[swift] test requirement \(label)")
  try await t.tell(3)
  print("[swift] awaited tell(3)")
  try send t.tell(4)
}

@main struct Main {
  static func main() async throws {
    FakeRecipientTargetPrinting.isEnabled = true
    let local = Worker(actorSystem: RecordingSystem())
    try await test(local, "local")
    // CHECK-LABEL: [swift] test local
    // CHECK-NOT: system.
    // CHECK: Worker.tell(1)
    // CHECK-NEXT: [swift] awaited tell(1)
    // CHECK-NOT: system.
    // CHECK: Worker.tell(2)
    // CHECK-NOT: system.
    // CHECK: [swift] sent tell(2)

    let system = RecordingSystem()
    let host = Worker(actorSystem: system)
    let remote = try Worker.resolve(id: host.id, using: system)
    try await test(remote, "remote")
    // CHECK-LABEL: [swift] test remote
    // CHECK: system.remoteCallVoid $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK: >> executeDistributedTarget: $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK: Worker.tell(1)
    // CHECK: system.remoteCallVoid returned
    // CHECK-NEXT: [swift] awaited tell(1)
    // CHECK: system.remoteCallOneway $s4main6WorkerC4tellyySiKFTE oneway: true target: true
    // CHECK: >> executeDistributedTarget: $s4main6WorkerC4tellyySiKFTE oneway: true target: true
    // CHECK: Worker.tell(2)
    // CHECK: [swift] sent tell(2)

    try await testThroughRequirement(remote, "remote")
    try await waitUntil { try await remote.count() >= 4 }
    // CHECK-LABEL: [swift] test requirement remote
    // CHECK: system.remoteCallVoid $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK: >> executeDistributedTarget: $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK: Worker.tell(3)
    // CHECK: system.remoteCallVoid returned
    // CHECK-NEXT: [swift] awaited tell(3)
    // CHECK: system.remoteCallOneway $s4main6WorkerC4tellyySiKFTE oneway: true target: true
    // CHECK: >> executeDistributedTarget: $s4main6WorkerC4tellyySiKFTE oneway: true target: true
    // CHECK: Worker.tell(4)

    // Through an existential, and through the '@Resolvable' stub, which the
    // recipient resolves through the requirement
    let existential: any Teller = remote
    try await existential.tell(5)
    print("[swift] awaited existential tell(5)")
    let stub = try $Teller.resolve(id: host.id, using: system)
    try await stub.tell(6)
    print("[swift] awaited stub tell(6)")
    try send stub.tell(7)
    try await waitUntil { try await remote.count() >= 7 }
    print("[swift] sent stub tell(7)")
    // CHECK: system.remoteCallVoid $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK: >> executeDistributedTarget: $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK: Worker.tell(5)
    // CHECK: system.remoteCallVoid returned
    // CHECK-NEXT: [swift] awaited existential tell(5)
    // CHECK: system.remoteCallVoid $s4main7$TellerC4tellyySiKFTE oneway: false target: true
    // CHECK: >> executeDistributedTarget: $s4main7$TellerC4tellyySiKFTE oneway: false target: true
    // CHECK: Worker.tell(6)
    // CHECK: system.remoteCallVoid returned
    // CHECK-NEXT: [swift] awaited stub tell(6)
    // CHECK: system.remoteCallOneway $s4main7$TellerC4tellyySiKFTE oneway: true target: true
    // CHECK: >> executeDistributedTarget: $s4main7$TellerC4tellyySiKFTE oneway: true target: true
    // CHECK: Worker.tell(7)
    // CHECK: [swift] sent stub tell(7)

    // An error thrown by 'remoteCallVoid' is thrown to the caller
    system.failAwaitedCalls = true
    print("[swift] test failing")
    do {
      try await remote.tell(8)
      print("[swift] FAIL: no error")
    } catch {
      print("[swift] caught \(error)")
    }
    do {
      try await testThroughRequirement(remote, "failing")
      print("[swift] FAIL: no error")
    } catch {
      print("[swift] caught through requirement \(error)")
    }
    do {
      try await stub.tell(9)
      print("[swift] FAIL: no error")
    } catch {
      print("[swift] caught through stub \(error)")
    }
    print("[swift] count \(try await remote.count())")
    // CHECK-LABEL: [swift] test failing
    // CHECK: system.remoteCallVoid $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK-NOT: Worker.tell
    // CHECK: [swift] caught TransportError()
    // CHECK: [swift] test requirement failing
    // CHECK: system.remoteCallVoid $s4main6WorkerC4tellyySiKFTE oneway: false target: true
    // CHECK-NOT: Worker.tell
    // CHECK: [swift] caught through requirement TransportError()
    // CHECK: system.remoteCallVoid $s4main7$TellerC4tellyySiKFTE oneway: false target: true
    // CHECK-NOT: Worker.tell
    // CHECK: [swift] caught through stub TransportError()
    // CHECK: [swift] count 7
  }
}
