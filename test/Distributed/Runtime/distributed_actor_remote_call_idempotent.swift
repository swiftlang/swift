// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -plugin-path %swift-plugin-dir -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: distributed
// REQUIRES: swift_swift_parser

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// The distributed thunks of an '@_remoteCallIdempotent' function mark the
// target of their remote calls with 'isIdempotentTarget', also when called
// through a '@Resolvable' stub, which copies the attribute from the protocol
// requirement. An actor system may then make such a call more than once: this
// one sends every call of an idempotent target twice, and any other call once

@_spi(DistributedIdempotent) import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = TwiceIfIdempotentSystem

/// Sends every call of an idempotent target twice, through a
/// 'FakeRoundtripActorSystem'. It resolves every reference as remote
final class TwiceIfIdempotentSystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = ActorAddress
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeRoundtripResultHandler

  let inner = FakeRoundtripActorSystem()

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

  /// How many times to send a call of 'target'
  ///
  /// Each send gets its own copy of the invocation, because 'makeDecoder()'
  /// of the fake encoder resets it
  func sendCount(_ target: RemoteCallTarget) -> Int {
    target.isIdempotentTarget ? 2 : 1
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type, returning returnType: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error,
            Res: SerializationRequirement {
    print("[swift] \(#fileID):\(#line) system.remoteCall idempotent: \(target.isIdempotentTarget)")
    var result: Res? = nil
    for _ in 0 ..< sendCount(target) {
      var copy = invocation
      result = try await inner.remoteCall(
        on: actor, target: target, invocation: &copy,
        throwing: errorType, returning: returnType)
    }
    return result!
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act, target: RemoteCallTarget,
    invocation: inout InvocationEncoder, throwing errorType: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    print("[swift] \(#fileID):\(#line) system.remoteCallVoid idempotent: \(target.isIdempotentTarget)")
    for _ in 0 ..< sendCount(target) {
      var copy = invocation
      try await inner.remoteCallVoid(
        on: actor, target: target, invocation: &copy,
        throwing: errorType)
    }
  }
}

@Resolvable
protocol Store: DistributedActor where ActorSystem == TwiceIfIdempotentSystem {
  @_remoteCallIdempotent distributed func put(_ key: String, _ value: Int)
}

distributed actor KVStore: Store {
  var values: [String: Int] = [:]
  var counter = 0

  @_remoteCallIdempotent
  distributed func put(_ key: String, _ value: Int) {
    values[key] = value
    print("[swift] \(#fileID):\(#line) KVStore.put(\(key), \(value))")
  }

  @_remoteCallIdempotent
  distributed func get(_ key: String) -> Int {
    print("[swift] \(#fileID):\(#line) KVStore.get(\(key))")
    return values[key, default: 0]
  }

  distributed func increment() -> Int {
    counter += 1
    print("[swift] \(#fileID):\(#line) KVStore.increment() -> \(counter)")
    return counter
  }
}

@main struct Main {
  static func main() async throws {
    let system = TwiceIfIdempotentSystem()
    let local = KVStore(actorSystem: system)
    let remote = try KVStore.resolve(id: local.id, using: system)

    print("[swift] idempotent call")
    try await remote.put("a", 1)
    // CHECK-LABEL: [swift] idempotent call
    // CHECK: system.remoteCallVoid idempotent: true
    // CHECK: KVStore.put(a, 1)
    // CHECK: KVStore.put(a, 1)
    // CHECK-NOT: KVStore.put

    print("[swift] idempotent call with a result")
    let value = try await remote.get("a")
    print("[swift] get(a) = \(value)")
    // CHECK-LABEL: [swift] idempotent call with a result
    // CHECK: system.remoteCall idempotent: true
    // CHECK: KVStore.get(a)
    // CHECK: KVStore.get(a)
    // CHECK-NOT: KVStore.get
    // CHECK: [swift] get(a) = 1

    print("[swift] call which is not idempotent")
    let count = try await remote.increment()
    print("[swift] increment() = \(count)")
    // CHECK-LABEL: [swift] call which is not idempotent
    // CHECK: system.remoteCall idempotent: false
    // CHECK: KVStore.increment() -> 1
    // CHECK-NOT: KVStore.increment
    // CHECK: [swift] increment() = 1

    print("[swift] idempotent call through a stub")
    let stub = try $Store.resolve(id: local.id, using: system)
    try await stub.put("b", 2)
    // CHECK-LABEL: [swift] idempotent call through a stub
    // CHECK: system.remoteCallVoid idempotent: true
    // CHECK: KVStore.put(b, 2)
    // CHECK: KVStore.put(b, 2)
    // CHECK-NOT: KVStore.put

    // A local call is a plain call, made once
    print("[swift] local call")
    try await local.put("c", 3)
    // CHECK-LABEL: [swift] local call
    // CHECK-NOT: system.
    // CHECK: KVStore.put(c, 3)
    // CHECK-NOT: KVStore.put
    // CHECK: [swift] done
    print("[swift] done")
  }
}
