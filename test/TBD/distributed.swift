// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-testing -enable-experimental-distributed -disable-availability-checking -emit-ir -o %t/test.ll -emit-tbd -emit-tbd-path %t/test.tbd -I %t
// RUN cat %t/test.tbd | %FileCheck %s --dump-input=always

import _Distributed

// CHECK: @"$s4test1AC13_remote_helloyyYaKFTE" = hidden global %swift.async_func_pointer
// CHECK: @"$s4test1AC13_remote_helloyyYaKFTETu" = hidden global %swift.async_func_pointer
distributed actor SomeDistributedActor {
  typealias ActorSystem = FakeActorSystem
  distributed func hello(name: String) -> String {
    "Hello, \(name)!"
  }
}

// function:
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tF
// function method descriptor
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTq
// thunk, method reference
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTE
// thunk, method reference + async function pointer
// IR unmangledName = $s4test20SomeDistributedActorC5hello4nameS2S_tFTETu

// ==== Fake Address -----------------------------------------------------------

public struct ActorAddress: Hashable, Sendable, Codable {
  public let address: String
  public init(parse address : String) {
    self.address = address
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    self.address = try container.decode(String.self)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(self.address)
  }
}

// ==== Fake Transport ---------------------------------------------------------

public struct FakeActorSystem: DistributedActorSystem {
  public typealias ActorID = ActorAddress
  public typealias Invocation = FakeInvocation
  public typealias SerializationRequirement = Codable

  init() {
    print("Initialized new FakeActorSystem")
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
      Act.ID == ActorID  {
    nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor,
      Act.ID == ActorID {
    ActorAddress(parse: "xxx")
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor,
      Act.ID == ActorID {
  }

  public func resignID(_ id: ActorID) {
  }

  public func makeInvocation() -> Invocation {
    .init()
  }
}

public struct FakeInvocation: DistributedTargetInvocation {
  public typealias ArgumentDecoder = FakeArgumentDecoder
  public typealias SerializationRequirement = Codable

  public mutating func recordGenericSubstitution<T>(mangledType: T.Type) throws {}
  public mutating func recordArgument<Argument: SerializationRequirement>(argument: Argument) throws {}
  public mutating func recordReturnType<R: SerializationRequirement>(mangledType: R.Type) throws {}
  public mutating func recordErrorType<E: Error>(mangledType: E.Type) throws {}
  public mutating func doneRecording() throws {}

  // === Receiving / decoding -------------------------------------------------

  public mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  public mutating func argumentDecoder() -> FakeArgumentDecoder { .init() }
  public mutating func decodeReturnType() throws -> Any.Type? { nil }
  public mutating func decodeErrorType() throws -> Any.Type? { nil }

  public struct FakeArgumentDecoder: DistributedTargetInvocationArgumentDecoder {
    public typealias SerializationRequirement = Codable
  }
}
