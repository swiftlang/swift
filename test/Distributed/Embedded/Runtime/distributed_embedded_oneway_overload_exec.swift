// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -module-name main -plugin-path %swift-plugin-dir %s %S/Inputs/EmbeddedFakeActorSystem.swift -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %target-swift-frontend -O -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -module-name main -plugin-path %swift-plugin-dir %s %S/Inputs/EmbeddedFakeActorSystem.swift -c -o %t/aopt.o
// RUN: %target-embedded-link %t/aopt.o %target-embedded-posix-shim -o %t/aopt.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/aopt.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait

// In Embedded Swift 'func f()' and 'func f() oneway' are distinct overloads:
// 'await x.f()' calls the two-way one and 'try nowait x.f()' (or 'nowait x.f()'
// for a non-distributed actor) calls the 'oneway' one, which is lowered
// synchronously. This holds for a local and a remote distributed actor, with a
// system which implements 'remoteCallVoidOneway' and with one which relies on
// its default implementation, for a plain actor and for a global actor. The
// two overloads have distinct remote call targets, and the receive dispatcher
// routes each of them to the right function

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

// Whether the identifier contains the 'Yo' ('oneway') mangling operator
func hasYo(_ bytes: [UInt8]) -> Bool {
  if bytes.count < 2 { return false }
  for i in 0 ..< bytes.count - 1 where bytes[i] == 0x59 && bytes[i + 1] == 0x6F {
    return true
  }
  return false
}

// The identifiers of all the remote call targets which were sent
nonisolated(unsafe) var sentIdentifiers: [[UInt8]] = []

func record(_ what: StaticString, _ target: borrowing RemoteCallTarget) -> [UInt8] {
  let bytes = target.identifier.withUnsafeBytes { unsafe [UInt8]($0) }
  var isNew = true
  for seen in sentIdentifiers where seen == bytes {
    isNew = false
  }
  if isNew { sentIdentifiers.append(bytes) }
  print("[swift] \(what) oneway: \(target.isOnewayRemoteCall) Yo: \(hasYo(bytes)) new: \(isNew)")
  return bytes
}

// ==== -----------------------------------------------------------------------
// MARK: An actor system which implements 'remoteCallVoidOneway'

struct SyncResultHandler: DistributedTargetInvocationResultHandler {
  func onReturnVoid() async throws {}
  func onThrow(error: any Error) async throws { print("[swift] onThrow") }
}
extension SyncResultHandler {
  func onReturn<Success: EmbeddedSerializationRequirement>(value: Success) async throws {}
}

final class SyncOnewaySystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = EmbeddedFakeActorID
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  typealias InvocationEncoder = EmbeddedFakeInvocationEncoder
  typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  typealias ResultHandler = SyncResultHandler

  typealias LocalDispatch =
    (borrowing RemoteCallTarget, inout InvocationDecoder, ResultHandler) async throws -> Void

  var active: [ActorID: LocalDispatch] = [:]
  var nextID: UInt64 = 1

  init() {}

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {
    return nil // always remote
  }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem {
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
    _ = record("remoteCallVoid", target)
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes
    var decoder = InvocationDecoder(buffer: requestBuffer)
    try await dispatch(target, &decoder, ResultHandler())
  }

  func remoteCallVoidOneway<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) throws
      where Act: DistributedActor, Act.ActorSystem == SyncOnewaySystem, Err: Error {
    let identifierBytes = record("remoteCallVoidOneway", target)
    guard let dispatch = active[actor.id] else {
      fatalError("no local actor hosted for the target id")
    }
    let requestBuffer = CallBuffer()
    requestBuffer.argBytes = invocation.buffer.argBytes
    _ = Task.immediate {
      let receivedTarget = RemoteCallTarget(identifierBytes.span.bytes)
      var decoder = InvocationDecoder(buffer: requestBuffer)
      try await dispatch(receivedTarget, &decoder, ResultHandler())
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Overloads which only differ in 'oneway'

// 'EmbeddedFakeRoundtripActorSystem' has no 'remoteCallVoidOneway', so it
// relies on the default implementation, which sends through 'remoteCallVoid'
distributed actor DefaultGreeter {
  typealias ActorSystem = EmbeddedFakeRoundtripActorSystem

  var ran = 0

  distributed func hello() { ran += 1; print("[swift] DefaultGreeter.hello two-way") }
  distributed func hello() oneway { ran += 1; print("[swift] DefaultGreeter.hello oneway") }

  distributed func greet(_ n: Int) { ran += 1; print("[swift] DefaultGreeter.greet two-way \(n)") }
  distributed func greet(_ n: Int) oneway { ran += 1; print("[swift] DefaultGreeter.greet oneway \(n)") }

  distributed func count() -> Int { ran }
}

distributed actor SyncGreeter {
  typealias ActorSystem = SyncOnewaySystem

  var ran = 0

  distributed func hello() { ran += 1; print("[swift] SyncGreeter.hello two-way") }
  distributed func hello() oneway { ran += 1; print("[swift] SyncGreeter.hello oneway") }

  distributed func greet(_ n: Int) { ran += 1; print("[swift] SyncGreeter.greet two-way \(n)") }
  distributed func greet(_ n: Int) oneway { ran += 1; print("[swift] SyncGreeter.greet oneway \(n)") }

  distributed func count() -> Int { ran }
}

actor Plain {
  var ran = 0
  func f() { ran += 1; print("[swift] Plain.f two-way") }
  func f() oneway { ran += 1; print("[swift] Plain.f oneway") }
  func count() -> Int { ran }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background var backgroundRan = 0

@Background func note() { backgroundRan += 1; print("[swift] note two-way") }
@Background func note() oneway { backgroundRan += 1; print("[swift] note oneway") }

@Background func getBackgroundRan() -> Int { backgroundRan }

// ==== -----------------------------------------------------------------------
// MARK: Synchronous senders

func sendDefault(_ g: DefaultGreeter) throws {
  try nowait g.hello()
}
func sendDefaultGreet(_ g: DefaultGreeter) throws {
  try nowait g.greet(2)
}
func sendSync(_ g: SyncGreeter) throws {
  try nowait g.hello()
}
func sendSyncGreet(_ g: SyncGreeter) throws {
  try nowait g.greet(2)
}
func sendPlain(_ p: Plain) {
  nowait p.f()
}
func sendNote() {
  nowait note()
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

// Calls through 'g' and waits on the local actor 'host' for the 'oneway' calls
func test_default(_ g: DefaultGreeter, host: DefaultGreeter, _ label: StaticString) async throws {
  print("[swift] test_default \(label)")
  try await g.hello()
  try sendDefault(g)
  try await waitUntil { try await host.count() >= 2 }
  try await g.greet(1)
  try sendDefaultGreet(g)
  try await waitUntil { try await host.count() >= 4 }
}

func test_sync(_ g: SyncGreeter, host: SyncGreeter, _ label: StaticString) async throws {
  print("[swift] test_sync \(label)")
  try await g.hello()
  try sendSync(g)
  try await waitUntil { try await host.count() >= 2 }
  try await g.greet(1)
  try sendSyncGreet(g)
  try await waitUntil { try await host.count() >= 4 }
}

@main struct Main {
  static func main() async {
    do {
      let defaultSystem = EmbeddedFakeRoundtripActorSystem()
      let defaultLocal = DefaultGreeter(actorSystem: defaultSystem)
      try await test_default(defaultLocal, host: defaultLocal, "local")
      // CHECK-LABEL: [swift] test_default local
      // CHECK-NEXT: [swift] DefaultGreeter.hello two-way
      // CHECK-NEXT: [swift] DefaultGreeter.hello oneway
      // CHECK-NEXT: [swift] DefaultGreeter.greet two-way 1
      // CHECK-NEXT: [swift] DefaultGreeter.greet oneway 2

      let defaultRemote = try DefaultGreeter.resolve(id: defaultLocal.id, using: defaultSystem)
      try await test_default(defaultRemote, host: defaultLocal, "remote")
      // CHECK-LABEL: [swift] test_default remote
      // CHECK-NEXT: [swift] remoteCallVoid reached
      // CHECK-NEXT: [swift] DefaultGreeter.hello two-way
      // CHECK: [swift] remoteCallVoid reached
      // CHECK: [swift] DefaultGreeter.hello oneway
      // CHECK: [swift] remoteCallVoid reached
      // CHECK-NEXT: [swift] DefaultGreeter.greet two-way 1
      // CHECK: [swift] remoteCallVoid reached
      // CHECK: [swift] DefaultGreeter.greet oneway 2

      let syncSystem = SyncOnewaySystem()
      let syncLocal = SyncGreeter(actorSystem: syncSystem)
      try await test_sync(syncLocal, host: syncLocal, "local")
      // CHECK-LABEL: [swift] test_sync local
      // CHECK-NEXT: [swift] SyncGreeter.hello two-way
      // CHECK-NEXT: [swift] SyncGreeter.hello oneway
      // CHECK-NEXT: [swift] SyncGreeter.greet two-way 1
      // CHECK-NEXT: [swift] SyncGreeter.greet oneway 2

      let syncRemote = try SyncGreeter.resolve(id: syncLocal.id, using: syncSystem)
      try await test_sync(syncRemote, host: syncLocal, "remote")
      // Each of the four targets is distinct, and only the 'oneway' ones are
      // flagged and mangled with 'Yo'
      // CHECK-LABEL: [swift] test_sync remote
      // CHECK-NEXT: [swift] remoteCallVoid oneway: false Yo: false new: true
      // CHECK-NEXT: [swift] SyncGreeter.hello two-way
      // CHECK-NEXT: [swift] remoteCallVoidOneway oneway: true Yo: true new: true
      // CHECK-NEXT: [swift] SyncGreeter.hello oneway
      // CHECK-NEXT: [swift] remoteCallVoid oneway: false Yo: false new: true
      // CHECK-NEXT: [swift] SyncGreeter.greet two-way 1
      // CHECK-NEXT: [swift] remoteCallVoidOneway oneway: true Yo: true new: true
      // CHECK-NEXT: [swift] SyncGreeter.greet oneway 2

      print("[swift] test_actor")
      let plain = Plain()
      await plain.f()
      sendPlain(plain)
      await waitUntil { await plain.count() >= 2 }
      // CHECK-LABEL: [swift] test_actor
      // CHECK-NEXT: [swift] Plain.f two-way
      // CHECK-NEXT: [swift] Plain.f oneway

      print("[swift] test_global_actor")
      await note()
      sendNote()
      await waitUntil { await getBackgroundRan() >= 2 }
      // CHECK-LABEL: [swift] test_global_actor
      // CHECK-NEXT: [swift] note two-way
      // CHECK-NEXT: [swift] note oneway
    } catch {
      print("[swift] threw")
    }
    print("[swift] done")
    // CHECK-NOT: [swift] threw
    // CHECK: [swift] done
  }
}
