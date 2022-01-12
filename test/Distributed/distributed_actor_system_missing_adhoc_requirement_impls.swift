// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Fake Transport ---------------------------------------------------------
struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

struct FakeActorSystem: DistributedActorSystem {
  // expected-error@-1{{struct 'FakeActorSystem' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'FakeActorSystem' requires function 'remoteCall' with signature:}}

  typealias ActorID = ActorAddress
  typealias Invocation = FakeInvocation
  typealias SerializationRequirement = Codable

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    let id = ActorAddress(parse: "fake://123")
    return id
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
  }

  func makeInvocation() -> Invocation {
    .init()
  }

}

struct FakeInvocation: DistributedTargetInvocation {
  typealias ArgumentDecoder = FakeArgumentDecoder
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(argument: Argument) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(mangledType: R.Type) throws {}
  mutating func recordErrorType<E: Error>(mangledType: E.Type) throws {}
  mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func makeArgumentDecoder() -> FakeArgumentDecoder { .init() }
  mutating func decodeReturnType() throws -> Any.Type? { nil }
  mutating func decodeErrorType() throws -> Any.Type? { nil }

  struct FakeArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
    typealias SerializationRequirement = FakeInvocation.SerializationRequirement
    mutating func decodeNext<Argument>(
      _ argumentType: Argument.Type,
      into pointer: UnsafeMutablePointer<Argument>
    ) throws {}
  }
}

@available(SwiftStdlib 5.5, *)
struct FakeResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = FakeInvocation.SerializationRequirement

  func onReturn<Res>(value: Res) async throws {
    print("RETURN: \(value)")
  }
  func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}

