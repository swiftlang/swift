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

// The runtime semantics of 'try nowait' on a synchronous 'oneway' distributed
// func in Embedded Swift, which calls the func's synchronous distributed thunk:
// - the receiver and the arguments are evaluated at the call site, in order,
//   exactly once, for a local and for a remote actor
// - a call from the actor itself is enqueued and runs after the current job
// - the local actor is kept alive until the call ran
// - the call runs in a new task which copies the caller's task locals,
//   inherits its priority and is not cancelled with the caller
// - errors thrown while encoding the call or by 'remoteCallVoidOneway'
//   propagate synchronously out of 'try nowait'; the 'Task.immediate' default
//   of 'remoteCallVoidOneway' drops the errors of 'remoteCallVoid'
// - calls through a generic, opaque or existential distributed protocol, and a
//   '@Resolvable' stub

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
  @TaskLocal static var inner: Int = 0
}

func log(_ s: String) { print("[swift] \(s)") }
func arg(_ n: Int) -> Int { log("arg \(n)"); return n }

nonisolated(unsafe) var deinits = 0

enum ProbeError: Error {
  case encode
  case send
  case remote
}

// ==== -----------------------------------------------------------------------
// MARK: An actor system with a synchronous 'remoteCallVoidOneway'

// Fails to encode a negative 'Int' argument
struct ProbeEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  var encoded: [Int] = []
  mutating func doneRecording() throws {}
}
extension ProbeEncoder {
  mutating func recordArgument<Value: EmbeddedSerializationRequirement>(
      _ argument: RemoteCallArgument<Value>) throws {
    if let n = argument.value as? Int {
      if n < 0 { throw ProbeError.encode }
      encoded.append(n)
    }
  }
}

struct ProbeResultHandler: DistributedTargetInvocationResultHandler {
  func onReturnVoid() async throws {}
  func onThrow(error: any Error) async throws {}
}
extension ProbeResultHandler {
  func onReturn<Success: EmbeddedSerializationRequirement>(value: Success) async throws {}
}

final class ProbeSystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = EmbeddedFakeActorID
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  typealias InvocationEncoder = ProbeEncoder
  typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  typealias ResultHandler = ProbeResultHandler

  var nextID: UInt64 = 1
  // Throw from 'remoteCallVoidOneway'
  var failSend = false

  init() {}

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == ProbeSystem {
    return nil // always remote
  }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == ProbeSystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  // Does not retain the actor
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == ProbeSystem {}
  func resignID(_ id: ActorID) {}

  func makeInvocationEncoder() -> InvocationEncoder { .init() }

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

  func remoteCallVoidOneway<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) throws
      where Act: DistributedActor, Act.ActorSystem == ProbeSystem, Err: Error {
    log("remoteCallVoidOneway \(invocation.encoded.count) args, oneway: \(target.isOnewayRemoteCall)")
    for n in invocation.encoded {
      log("remoteCallVoidOneway arg \(n)")
    }
    if failSend { throw ProbeError.send }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: An actor system with only the default 'remoteCallVoidOneway'

final class DefaultOnewaySystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = EmbeddedFakeActorID
  typealias SerializationRequirement = EmbeddedSerializationRequirement
  typealias InvocationEncoder = ProbeEncoder
  typealias InvocationDecoder = EmbeddedFakeInvocationDecoder
  typealias ResultHandler = ProbeResultHandler

  var nextID: UInt64 = 1
  var calls = 0

  init() {}

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ActorSystem == DefaultOnewaySystem {
    return nil
  }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ActorSystem == DefaultOnewaySystem {
    defer { nextID += 1 }
    return ActorID(id: nextID)
  }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ActorSystem == DefaultOnewaySystem {}
  func resignID(_ id: ActorID) {}

  func makeInvocationEncoder() -> InvocationEncoder { .init() }

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

  // The remote call fails, and nobody can observe that
  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    calls += 1
    log("remoteCallVoid \(invocation.encoded.count) args, oneway: \(target.isOnewayRemoteCall), throwing")
    throw ProbeError.remote
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Actors and protocols

protocol Steppable: DistributedActor where ActorSystem == ProbeSystem {
  distributed func step(_ n: Int) oneway
}

@Resolvable
protocol RemoteSteppable: DistributedActor where ActorSystem == ProbeSystem {
  distributed func remoteStep(_ n: Int) oneway
}

final class Tracker {
  let name: String
  init(_ name: String) { self.name = name }
  deinit {
    log("deinit \(name)")
    deinits += 1
  }
}

distributed actor Stepper: Steppable, RemoteSteppable {
  typealias ActorSystem = ProbeSystem

  let tracker: Tracker?
  var count = 0

  init(tracker: Tracker? = nil, actorSystem: ProbeSystem) {
    self.tracker = tracker
    self.actorSystem = actorSystem
  }

  distributed func step(_ n: Int) oneway {
    count += 1
    log("step \(n)")
  }

  distributed func step2(_ a: Int, _ b: Int) oneway {
    count += 1
    log("step2 \(a) \(b)")
  }

  distributed func remoteStep(_ n: Int) oneway {
    count += 1
    log("remoteStep \(n)")
  }

  distributed func probe() oneway {
    count += 1
    log("probe tag \(Probe.tag) inner \(Probe.inner)")
    log("probe cancelled \(Task.isCancelled)")
    withUnsafeCurrentTask { t in log("probe has task \(t != nil)") }
    log("probe priority \(Task.currentPriority.rawValue)")
  }

  distributed func getCount() -> Int { count }

  distributed func selfSend() throws {
    try nowait self.step(1)
    try nowait step(2)
    log("selfSend end")
  }
}

distributed actor DefaultStepper {
  typealias ActorSystem = DefaultOnewaySystem

  distributed func step(_ n: Int) oneway {
    log("DefaultStepper.step \(n)")
  }
}

func waitFor(_ s: Stepper, _ n: Int) async throws {
  try await waitUntil { try await s.getCount() >= n }
}

func makeRef(_ s: Stepper) -> Stepper { log("receiver"); return s }

func sendGeneric<S: Steppable>(_ s: S, _ n: Int) throws {
  try nowait s.step(n)
}

func sendOpaque(_ s: some Steppable, _ n: Int) throws {
  try nowait s.step(n)
}

func sendExistential(_ s: any Steppable, _ n: Int) throws {
  try nowait s.step(n)
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

func test_evaluation() async throws {
  print("[swift] test_evaluation")
  let system = ProbeSystem()
  let local = Stepper(actorSystem: system)
  let remote = try Stepper.resolve(id: local.id, using: system)

  try nowait makeRef(local).step2(arg(1), arg(2))
  log("after local nowait")
  try await waitFor(local, 1)

  try nowait makeRef(remote).step2(arg(3), arg(4))
  log("after remote nowait")
}
// CHECK-LABEL: [swift] test_evaluation
// CHECK-NEXT: [swift] receiver
// CHECK-NEXT: [swift] arg 1
// CHECK-NEXT: [swift] arg 2
// CHECK-NEXT: [swift] after local nowait
// CHECK-NEXT: [swift] step2 1 2
// CHECK-NEXT: [swift] receiver
// CHECK-NEXT: [swift] arg 3
// CHECK-NEXT: [swift] arg 4
// The remote send is synchronous
// CHECK-NEXT: [swift] remoteCallVoidOneway 2 args, oneway: true
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 3
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 4
// CHECK-NEXT: [swift] after remote nowait

func test_ordering() async throws {
  print("[swift] test_ordering")
  let system = ProbeSystem()
  let local = Stepper(actorSystem: system)
  try nowait local.step(10)
  try nowait local.step(11)
  // FIFO with the following awaited call
  let n = try await local.getCount()
  log("count \(n)")

  try await local.selfSend()
  try await waitFor(local, 4)
}
// CHECK-LABEL: [swift] test_ordering
// CHECK-NEXT: [swift] step 10
// CHECK-NEXT: [swift] step 11
// CHECK-NEXT: [swift] count 2
// Enqueued, so they only run after the current method returned
// CHECK-NEXT: [swift] selfSend end
// CHECK-NEXT: [swift] step 1
// CHECK-NEXT: [swift] step 2

func test_lifetime() async throws {
  print("[swift] test_lifetime")
  let system = ProbeSystem()
  do {
    let local = Stepper(tracker: Tracker("stepper"), actorSystem: system)
    try nowait local.step(20)
  }
  log("scope exited")
  await waitUntil { deinits >= 1 }
}
// CHECK-LABEL: [swift] test_lifetime
// CHECK-NEXT: [swift] scope exited
// CHECK-NEXT: [swift] step 20
// CHECK-NEXT: [swift] deinit stepper

func test_task() async throws {
  print("[swift] test_task")
  let system = ProbeSystem()
  let local = Stepper(actorSystem: system)
  try await Probe.$tag.withValue(1) {
    try await Probe.$inner.withValue(2) {
      try await Probe.$tag.withValue(3) {
        let caller = Task(priority: .high) {
          withUnsafeCurrentTask { $0?.cancel() }
          log("caller cancelled \(Task.isCancelled)")
          log("caller priority \(Task.currentPriority.rawValue)")
          try nowait local.probe()
        }
        try await caller.value
      }
    }
  }
  try await waitFor(local, 1)
}
// CHECK-LABEL: [swift] test_task
// CHECK-NEXT: [swift] caller cancelled true
// CHECK-NEXT: [swift] caller priority 25
// CHECK-NEXT: [swift] probe tag 3 inner 2
// CHECK-NEXT: [swift] probe cancelled false
// CHECK-NEXT: [swift] probe has task true
// CHECK-NEXT: [swift] probe priority 25

func test_errors() async throws {
  print("[swift] test_errors")
  let system = ProbeSystem()
  let local = Stepper(actorSystem: system)
  let remote = try Stepper.resolve(id: local.id, using: system)

  // An encoding error propagates out of 'try nowait', nothing is sent
  do {
    try nowait remote.step2(1, -1)
    log("encode: no error")
  } catch ProbeError.encode {
    log("encode: caught")
  }

  // An error thrown by 'remoteCallVoidOneway' propagates out of 'try nowait'
  system.failSend = true
  do {
    try nowait remote.step(2)
    log("send: no error")
  } catch ProbeError.send {
    log("send: caught")
  }

  // The default 'remoteCallVoidOneway' drops the error of 'remoteCallVoid'
  let defaultSystem = DefaultOnewaySystem()
  let defaultLocal = DefaultStepper(actorSystem: defaultSystem)
  let defaultRemote = try DefaultStepper.resolve(id: defaultLocal.id, using: defaultSystem)
  do {
    try nowait defaultRemote.step(3)
    log("default: no error")
  } catch {
    log("default: caught")
  }
  await waitUntil { defaultSystem.calls >= 1 }
}
// CHECK-LABEL: [swift] test_errors
// CHECK-NEXT: [swift] encode: caught
// CHECK-NEXT: [swift] remoteCallVoidOneway 1 args, oneway: true
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 2
// CHECK-NEXT: [swift] send: caught
// 'Task.immediate' runs the default synchronously up to its first suspension
// CHECK-NEXT: [swift] remoteCallVoid 1 args, oneway: true, throwing
// CHECK-NEXT: [swift] default: no error

func test_protocols() async throws {
  print("[swift] test_protocols")
  let system = ProbeSystem()
  let local = Stepper(actorSystem: system)
  let remote = try Stepper.resolve(id: local.id, using: system)

  try sendGeneric(local, 30)
  try sendOpaque(local, 31)
  try sendExistential(local, 32)
  try await waitFor(local, 3)
  try sendGeneric(remote, 33)
  try sendOpaque(remote, 34)
  try sendExistential(remote, 35)

  // A '@Resolvable' stub is always remote
  let stub = try $RemoteSteppable.resolve(id: local.id, using: system)
  try nowait stub.remoteStep(36)
}
// CHECK-LABEL: [swift] test_protocols
// CHECK-NEXT: [swift] step 30
// CHECK-NEXT: [swift] step 31
// CHECK-NEXT: [swift] step 32
// CHECK-NEXT: [swift] remoteCallVoidOneway 1 args, oneway: true
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 33
// CHECK-NEXT: [swift] remoteCallVoidOneway 1 args, oneway: true
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 34
// CHECK-NEXT: [swift] remoteCallVoidOneway 1 args, oneway: true
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 35
// CHECK-NEXT: [swift] remoteCallVoidOneway 1 args, oneway: true
// CHECK-NEXT: [swift] remoteCallVoidOneway arg 36

@main struct Main {
  static func main() async {
    do {
      try await test_evaluation()
      try await test_ordering()
      try await test_lifetime()
      try await test_task()
      try await test_errors()
      try await test_protocols()
    } catch {
      print("[swift] threw")
    }
    print("[swift] done")
  }
}
// CHECK-NOT: [swift] threw
// CHECK: [swift] done
