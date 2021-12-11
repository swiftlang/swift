import _Distributed

/// Use the existential wrapper as the default actor system.
typealias DefaultDistributedActorSystem = FakeActorSystem

@available(SwiftStdlib 5.6, *)
public distributed actor DA {
  public distributed func doSomethingDistributed(param: String) async -> Int {
    return 0
  }
}

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

