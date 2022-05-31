// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

struct MissingRemoteCall: DistributedActorSystem {
  // expected-error@-1{{struct 'MissingRemoteCall' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  // expected-error@-4{{struct 'MissingRemoteCall' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-5{{protocol 'DistributedActorSystem' requires function 'remoteCallVoid' with signature:}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
  }

  func makeInvocationEncoder() -> InvocationEncoder {
  }
}

struct RemoteCallMutating: DistributedActorSystem {
  // expected-error@-1{{struct 'RemoteCallMutating' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  // expected-error@-4{{struct 'RemoteCallMutating' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-5{{protocol 'DistributedActorSystem' requires function 'remoteCallVoid' with signature:}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
  }

  mutating func remoteCall<Act, Err, Res>(
      on actor: Act,
      target: RemoteCallTarget,
      invocation: inout InvocationEncoder,
      throwing: Err.Type,
      returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor,
      Act.ID == ActorID,
      Err: Error,
      Res: SerializationRequirement {
    fatalError("NOT IMPLEMENTED \(#function)")
  }

  mutating func remoteCallVoid<Act, Err>(
      on actor: Act,
      target: RemoteCallTarget,
      invocation: inout InvocationEncoder,
      throwing: Err.Type
  ) async throws
      where Act: DistributedActor,
      Act.ID == ActorID,
      Err: Error {
    fatalError("NOT IMPLEMENTED \(#function)")
  }

  func makeInvocationEncoder() -> InvocationEncoder {
  }
}

struct MissingRemoteCall_missingInout_on_encoder: DistributedActorSystem {
  // expected-error@-1{{struct 'MissingRemoteCall_missingInout_on_encoder' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  // expected-error@-4{{struct 'MissingRemoteCall_missingInout_on_encoder' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-5{{protocol 'DistributedActorSystem' requires function 'remoteCallVoid' with signature:}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
  }

  func remoteCall<Act, Err, Res>(
      on actor: Act,
      target: RemoteCallTarget,
      invocation: InvocationEncoder, // MISSING 'inout'
      throwing: Err.Type,
      returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor,
      Act.ID == ActorID,
      Err: Error,
      Res: SerializationRequirement {
    fatalError("NOT IMPLEMENTED \(#function)")
  }

  func remoteCallVoid<Act, Err>(
      on actor: Act,
      target: RemoteCallTarget,
      invocation: InvocationEncoder, // MISSING 'inout'
      throwing: Err.Type
  ) async throws
      where Act: DistributedActor,
      Act.ID == ActorID,
      Err: Error {
    fatalError("NOT IMPLEMENTED \(#function)")
  }

  func makeInvocationEncoder() -> InvocationEncoder {
  }
}

struct MissingRemoteCall_missing_makeInvocationEncoder: DistributedActorSystem {
  // expected-error@-1{{type 'MissingRemoteCall_missing_makeInvocationEncoder' does not conform to protocol 'DistributedActorSystem'}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

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
          Res: SerializationRequirement {
    fatalError("NOT IMPLEMENTED \(#function)")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    fatalError("NOT IMPLEMENTED \(#function)")
  }

  func resignID(_ id: ActorID) {
  }

  // func makeInvocationEncoder() -> InvocationEncoder {} // MISSING
}

struct Error_wrongReturn: DistributedActorSystem {
  // expected-error@-1{{struct 'Error_wrongReturn' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  // expected-error@-4{{struct 'Error_wrongReturn' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-5{{protocol 'DistributedActorSystem' requires function 'remoteCallVoid' with signature:}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
  }

  func resignID(_ id: ActorID) {
  }

  func makeInvocationEncoder() -> InvocationEncoder {
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> String // ERROR: wrong return type
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error,
    Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws // ERROR: wrong return type (void)
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error,
    Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) throws -> String // ERROR: should not return anything
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

struct BadRemoteCall_param: DistributedActorSystem {
  // expected-error@-1{{struct 'BadRemoteCall_param' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  // expected-error@-4{{struct 'BadRemoteCall_param' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-5{{protocol 'DistributedActorSystem' requires function 'remoteCallVoid' with signature:}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = FakeInvocationDecoder
  typealias InvocationEncoder = FakeInvocationEncoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = FakeResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder {
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    // invocation invocationEncoder: inout InvocationEncoder, // ERROR: missing parameter
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error,
    Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    // invocation invocationEncoder: inout InvocationEncoder, // ERROR: missing parameter
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

public struct BadRemoteCall_notPublic: DistributedActorSystem {
  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = PublicFakeInvocationDecoder
  public typealias InvocationEncoder = PublicFakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeResultHandler

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  public func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {}
  public func resignID(_ id: ActorID) {}
  public func makeInvocationEncoder() -> InvocationEncoder {
  }

  // expected-error@+1{{method 'remoteCall(on:target:invocation:throwing:returning:)' must be as accessible as its enclosing type because it matches a requirement in protocol 'DistributedActorSystem'}}
  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  // expected-error@+1{{method 'remoteCallVoid(on:target:invocation:throwing:)' must be as accessible as its enclosing type because it matches a requirement in protocol 'DistributedActorSystem'}}
  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

public struct BadRemoteCall_badResultConformance: DistributedActorSystem {
  // expected-error@-1{{struct 'BadRemoteCall_badResultConformance' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = PublicFakeInvocationDecoder
  public typealias InvocationEncoder = PublicFakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeResultHandler

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  public func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {}
  public func resignID(_ id: ActorID) {}
  public func makeInvocationEncoder() -> InvocationEncoder {
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SomeProtocol { // ERROR: bad, this must be SerializationRequirement
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

// This tests the ability to handle composite types with multiple layers
// Codable & SomeProtocol -> Encodable & Decodable & SomeProtocol
struct BadRemoteCall_largeSerializationRequirement: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = LargeSerializationReqFakeInvocationDecoder
  typealias InvocationEncoder = LargeSerializationReqFakeInvocationEncoder
  typealias SerializationRequirement = Codable & SomeProtocol
  typealias ResultHandler = LargeSerializationReqFakeInvocationResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder {
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

struct BadRemoteCall_largeSerializationRequirementSlightlyOffInDefinition: DistributedActorSystem {
  // expected-error@-1{{struct 'BadRemoteCall_largeSerializationRequirementSlightlyOffInDefinition' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{protocol 'DistributedActorSystem' requires function 'remoteCall' with signature:}}

  typealias ActorID = ActorAddress
  typealias InvocationDecoder = LargeSerializationReqFakeInvocationDecoder
  typealias InvocationEncoder = LargeSerializationReqFakeInvocationEncoder
  typealias SerializationRequirement = Codable & SomeProtocol
  typealias ResultHandler = LargeSerializationReqFakeInvocationResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder {
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SomeProtocol { // ERROR: missing Codable!!!
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

struct BadRemoteCall_anySerializationRequirement: DistributedActorSystem {
  typealias ActorID = ActorAddress
  typealias InvocationDecoder = AnyInvocationDecoder
  typealias InvocationEncoder = AnyInvocationEncoder
  typealias SerializationRequirement = Any // !!
  typealias ResultHandler = AnyResultHandler

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    return nil
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    ActorAddress(parse: "fake://123")
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder {
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error,
          Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

// ==== ------------------------------------------------------------------------

public struct ActorAddress: Sendable, Hashable, Codable {
  let address: String
  init(parse address: String) {
    self.address = address
  }
}

public protocol SomeProtocol: Sendable {}

struct FakeInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct AnyInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Any

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct LargeSerializationReqFakeInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable & SomeProtocol

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

public struct PublicFakeInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  public mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  public mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordArgument: DistributedTargetInvocationEncoder {
  //expected-error@-1{{struct 'FakeInvocationEncoder_missing_recordArgument' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-2{{protocol 'DistributedTargetInvocationEncoder' requires function 'recordArgument' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  // MISSING: mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordArgument2: DistributedTargetInvocationEncoder {
  //expected-error@-1{{struct 'FakeInvocationEncoder_missing_recordArgument2' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-2{{protocol 'DistributedTargetInvocationEncoder' requires function 'recordArgument' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Argument: SomeProtocol>(_ argument: Argument) throws {} // BAD
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordReturnType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{struct 'FakeInvocationEncoder_missing_recordReturnType' is missing witness for protocol requirement 'recordReturnType'}}
  //expected-note@-2{{protocol 'DistributedTargetInvocationEncoder' requires function 'recordReturnType' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  // MISSING: mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordErrorType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{type 'FakeInvocationEncoder_missing_recordErrorType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  // MISSING: mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_recordArgument_wrongType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{struct 'FakeInvocationEncoder_recordArgument_wrongType' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-2{{protocol 'DistributedTargetInvocationEncoder' requires function 'recordArgument' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: String, other: Argument) throws {} // BAD
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: Argument.Type) throws {} // BAD
  mutating func recordArgument<Argument: SerializationRequirement>(badName: Argument) throws {} // BAD
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}
struct FakeInvocationEncoder_recordArgument_missingMutating: DistributedTargetInvocationEncoder {
  //expected-error@-1{{struct 'FakeInvocationEncoder_recordArgument_missingMutating' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-2{{protocol 'DistributedTargetInvocationEncoder' requires function 'recordArgument' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_recordResultType_wrongType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{struct 'FakeInvocationEncoder_recordResultType_wrongType' is missing witness for protocol requirement 'recordReturnType'}}
  //expected-note@-2{{protocol 'DistributedTargetInvocationEncoder' requires function 'recordReturnType' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(s: String, _ resultType: R.Type) throws {} // BAD
  mutating func recordReturnType<R: SomeProtocol>(_ resultType: R.Type) throws {} // BAD
  mutating func recordReturnType<R: SerializationRequirement>(badName: R.Type) throws {} // BAD
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_recordErrorType_wrongType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{type 'FakeInvocationEncoder_recordErrorType_wrongType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(BadName type: E.Type) throws {} // BAD
  mutating func recordErrorType<E: SerializationRequirement>(_ type: E.Type) throws {} // BAD
  //expected-note@-1{{candidate has non-matching type '<E> (E.Type) throws -> ()'}}
  mutating func doneRecording() throws {}
}

class FakeInvocationDecoder: DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

class AnyInvocationDecoder: DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Any

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

class LargeSerializationReqFakeInvocationDecoder : DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable & SomeProtocol

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

public final class PublicFakeInvocationDecoder : DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  public func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  public func decodeReturnType() throws -> Any.Type? { nil }
  public func decodeErrorType() throws -> Any.Type? { nil }
}

public final class PublicFakeInvocationDecoder_badNotPublic: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  public func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument { fatalError() }
  // expected-error@-1{{method 'decodeNextArgument()' must be as accessible as its enclosing type because it matches a requirement in protocol 'DistributedTargetInvocationDecoder'}}
  func decodeReturnType() throws -> Any.Type? { nil }
  // expected-error@-1{{method 'decodeReturnType()' must be declared public because it matches a requirement in public protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-2{{mark the instance method as 'public' to satisfy the requirement}}
  func decodeErrorType() throws -> Any.Type? { nil }
  // expected-error@-1{{method 'decodeErrorType()' must be declared public because it matches a requirement in public protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-2{{mark the instance method as 'public' to satisfy the requirement}}
}

final class PublicFakeInvocationDecoder_badBadProtoRequirement: DistributedTargetInvocationDecoder {
  // expected-error@-1{{class 'PublicFakeInvocationDecoder_badBadProtoRequirement' is missing witness for protocol requirement 'decodeNextArgument'}}
  // expected-note@-2{{protocol 'DistributedTargetInvocationDecoder' requires function 'decodeNextArgument' with signature:}}
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  func decodeNextArgument<Argument: SomeProtocol>() throws -> Argument { fatalError() }
  func decodeReturnType() throws -> Any.Type? { nil }
  func decodeErrorType() throws -> Any.Type? { nil }
}

public struct FakeResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print("RETURN: \(value)")
  }
  public func onReturnVoid() async throws {
    print("RETURN VOID")
  }
  public func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}

struct AnyResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = Any

  func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print("RETURN: \(value)")
  }
  func onReturnVoid() async throws {
    print("RETURN VOID")
  }
  func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}
struct LargeSerializationReqFakeInvocationResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = Codable & SomeProtocol

  func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print("RETURN: \(value)")
  }
  func onReturnVoid() async throws {
    print("RETURN VOID")
  }
  func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}

struct BadResultHandler_missingOnReturn: DistributedTargetInvocationResultHandler {
  // expected-error@-1{{struct 'BadResultHandler_missingOnReturn' is missing witness for protocol requirement 'onReturn'}}
  // expected-note@-2{{protocol 'DistributedTargetInvocationResultHandler' requires function 'onReturn' with signature:}}
  typealias SerializationRequirement = Codable

  // func onReturn<Res: SerializationRequirement>(value: Res) async throws {} // MISSING
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}

struct BadResultHandler_missingRequirement: DistributedTargetInvocationResultHandler {
  // expected-error@-1{{struct 'BadResultHandler_missingRequirement' is missing witness for protocol requirement 'onReturn'}}
  // expected-note@-2{{protocol 'DistributedTargetInvocationResultHandler' requires function 'onReturn' with signature:}}
  typealias SerializationRequirement = Codable

  func onReturn<Success>(value: Success) async throws {} // MISSING : Codable
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}

struct BadResultHandler_mutatingButShouldNotBe: DistributedTargetInvocationResultHandler {
  // expected-error@-1{{struct 'BadResultHandler_mutatingButShouldNotBe' is missing witness for protocol requirement 'onReturn'}}
  // expected-note@-2{{protocol 'DistributedTargetInvocationResultHandler' requires function 'onReturn' with signature:}}
  typealias SerializationRequirement = Codable

  mutating func onReturn<Success: Codable>(value: Success) async throws {} // WRONG: can't be mutating
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}

public struct PublicFakeResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print("RETURN: \(value)")
  }
  public func onReturnVoid() async throws {
    print("RETURN VOID")
  }
  public func onThrow<Err: Error>(error: Err) async throws {
    print("ERROR: \(error)")
  }
}

