// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature OnewayNowait -Xfrontend -disable-experimental-parser-round-trip -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// 'func f()' and 'func f() oneway' are distinct overloads: 'await x.f()' calls
// the two-way one and 'nowait x.f()' calls the 'oneway' one. This holds for a
// local and a remote distributed actor, through a distributed protocol, for a
// plain actor and for a global actor. For a remote call the two overloads have
// distinct remote call targets, and only the 'oneway' one is flagged as such

import Distributed
import FakeDistributedActorSystems

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

// ==== -----------------------------------------------------------------------
// MARK: A roundtrip system which records the remote call targets

final class RecordingSystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = ActorAddress
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeRoundtripResultHandler

  let inner = FakeRoundtripActorSystem()

  func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor {
    nil // always remote
  }
  func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor {
    inner.assignID(actorType)
  }
  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    inner.actorReady(actor)
  }
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder { .init() }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type,
    returning returnType: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error,
            Res: SerializationRequirement {
    try await inner.remoteCall(
      on: actor, target: target, invocation: &invocation,
      throwing: errorType, returning: returnType)
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    let isOneway = target.isOnewayRemoteCall
    let hasYo = target.identifier.contains("Yo")
    print("[swift] target \(target.identifier) oneway: \(isOneway) Yo: \(hasYo)")
    try await inner.remoteCallVoid(
      on: actor, target: target, invocation: &invocation,
      throwing: errorType)
  }
}

typealias DefaultDistributedActorSystem = RecordingSystem

// ==== -----------------------------------------------------------------------
// MARK: Overloads which only differ in 'oneway'

protocol Speaker: DistributedActor where ActorSystem == RecordingSystem {
  distributed func speak()
  distributed func speak() oneway
}

distributed actor Greeter: Speaker {
  var ran: [String] = []

  distributed func hello() { ran.append("hello"); print("[swift] hello two-way") }
  distributed func hello() oneway { ran.append("hello oneway"); print("[swift] hello oneway") }

  distributed func greet(_ name: String) async {
    ran.append("greet"); print("[swift] greet two-way \(name)")
  }
  distributed func greet(_ name: String) async oneway {
    ran.append("greet oneway"); print("[swift] greet oneway \(name)")
  }

  distributed func speak() { ran.append("speak"); print("[swift] speak two-way") }
  distributed func speak() oneway { ran.append("speak oneway"); print("[swift] speak oneway") }

  distributed func count() -> Int { ran.count }
}

actor Plain {
  var ran = 0
  func f() { ran += 1; print("[swift] Plain.f two-way") }
  func f() oneway { ran += 1; print("[swift] Plain.f oneway") }
  func count() -> Int { ran }
}

@MainActor final class Screen {
  var ran = 0
  func f() { ran += 1; print("[swift] Screen.f two-way") }
  func f() oneway { ran += 1; print("[swift] Screen.f oneway") }
}

func speakBoth<S: Speaker>(_ s: S) async throws {
  try await s.speak()
  nowait s.speak()
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

func test_distributed(_ g: Greeter, _ label: String) async throws {
  print("[swift] test_distributed \(label)")
  try await g.hello()
  nowait g.hello()
  try await waitUntil { try await g.count() >= 2 }
  try await g.greet("a")
  nowait g.greet("b")
  try await waitUntil { try await g.count() >= 4 }
  try await speakBoth(g)
  try await waitUntil { try await g.count() >= 6 }
}

@main struct Main {
  static func main() async throws {
    let system = RecordingSystem()

    let local = Greeter(actorSystem: system)
    try await test_distributed(local, "local")
    // CHECK-LABEL: [swift] test_distributed local
    // CHECK-NOT: [swift] target
    // CHECK: [swift] hello two-way
    // CHECK-NEXT: [swift] hello oneway
    // CHECK-NEXT: [swift] greet two-way a
    // CHECK-NEXT: [swift] greet oneway b
    // CHECK-NEXT: [swift] speak two-way
    // CHECK-NEXT: [swift] speak oneway

    let host = Greeter(actorSystem: system)
    let remote = try Greeter.resolve(id: host.id, using: system)
    try await test_distributed(remote, "remote")
    // CHECK-LABEL: [swift] test_distributed remote
    // CHECK: [swift] target $s4main7GreeterC5helloyyYaKFTE oneway: false Yo: false
    // CHECK: [swift] hello two-way
    // CHECK: [swift] target $s4main7GreeterC5helloyyYaYoKFTE oneway: true Yo: true
    // CHECK: [swift] hello oneway
    // CHECK: [swift] target $s4main7GreeterC5greetyySSYaKFTE oneway: false Yo: false
    // CHECK: [swift] greet two-way a
    // CHECK: [swift] target $s4main7GreeterC5greetyySSYaYoKFTE oneway: true Yo: true
    // CHECK: [swift] greet oneway b
    // CHECK: [swift] target {{.*}}5speak{{.*}} oneway: false Yo: false
    // CHECK: [swift] speak two-way
    // CHECK: [swift] target {{.*}}5speak{{.*}} oneway: true Yo: true
    // CHECK: [swift] speak oneway

    print("[swift] test_actor")
    let plain = Plain()
    await plain.f()
    nowait plain.f()
    await waitUntil { await plain.count() >= 2 }
    // CHECK-LABEL: [swift] test_actor
    // CHECK-NEXT: [swift] Plain.f two-way
    // CHECK-NEXT: [swift] Plain.f oneway

    print("[swift] test_global_actor")
    let screen = await Screen()
    await screen.f()
    nowait screen.f()
    await waitUntil { await screen.ran >= 2 }
    // CHECK-LABEL: [swift] test_global_actor
    // CHECK-NEXT: [swift] Screen.f two-way
    // CHECK-NEXT: [swift] Screen.f oneway

    print("[swift] done")
    // CHECK: [swift] done
  }
}
