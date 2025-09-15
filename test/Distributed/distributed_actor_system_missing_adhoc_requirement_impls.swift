// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

struct MissingRemoteCall: DistributedActorSystem { 
  // expected-error@-1 {{type 'MissingRemoteCall' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'MissingRemoteCall' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}
  // expected-error@-5{{struct 'MissingRemoteCall' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-6{{add stubs for conformance}}

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
  // expected-error@-1 {{type 'RemoteCallMutating' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'RemoteCallMutating' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}
  // expected-error@-5{{struct 'RemoteCallMutating' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-6{{add stubs for conformance}}

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

  // expected-note@+1 {{candidate is marked 'mutating' but protocol does not allow it}}
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

  // expected-note@+1 {{candidate is marked 'mutating' but protocol does not allow it}}
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
  // expected-error@-1 {{type 'MissingRemoteCall_missingInout_on_encoder' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'MissingRemoteCall_missingInout_on_encoder' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}
  // expected-error@-5{{struct 'MissingRemoteCall_missingInout_on_encoder' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-6{{add stubs for conformance}}

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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: MissingRemoteCall_missingInout_on_encoder.InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res' (aka '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: FakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res')}}
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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err> (on: Act, target: RemoteCallTarget, invocation: MissingRemoteCall_missingInout_on_encoder.InvocationEncoder, throwing: Err.Type) async throws -> ()' (aka '<Act, Err> (on: Act, target: RemoteCallTarget, invocation: FakeInvocationEncoder, throwing: Err.Type) async throws -> ()')}}
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
  // expected-note@-2{{add stubs for conformance}}

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
  // expected-error@-1{{type 'Error_wrongReturn' does not conform to protocol 'DistributedActorSystem'}}
  //expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'Error_wrongReturn' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}
  // expected-error@-5{{struct 'Error_wrongReturn' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-6{{add stubs for conformance}}

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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout Error_wrongReturn.InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> String' (aka '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> String')}}
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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout Error_wrongReturn.InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> ()' (aka '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> ()')}}
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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err> (on: Act, target: RemoteCallTarget, invocation: inout Error_wrongReturn.InvocationEncoder, throwing: Err.Type) throws -> String' (aka '<Act, Err> (on: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type) throws -> String')}}
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
  // expected-error@-1{{type 'BadRemoteCall_param' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'BadRemoteCall_param' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}
  // expected-error@-5{{struct 'BadRemoteCall_param' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-6{{add stubs for conformance}}

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

  // expected-error@+3{{method 'remoteCall(on:target:invocation:throwing:returning:)' must be as accessible as its enclosing type because it matches a requirement in protocol 'DistributedActorSystem'}}
  // expected-error@+2 {{method 'remoteCall(on:target:invocation:throwing:returning:)' must be declared public because it matches a requirement in public protocol 'DistributedActorSystem'}}
  // expected-note@+1 {{mark the instance method as 'public' to satisfy the requirement}}
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

  // expected-error@+3{{method 'remoteCallVoid(on:target:invocation:throwing:)' must be as accessible as its enclosing type because it matches a requirement in protocol 'DistributedActorSystem'}}
  // expected-error@+2 {{method 'remoteCallVoid(on:target:invocation:throwing:)' must be declared public because it matches a requirement in public protocol 'DistributedActorSystem'}}
  // expected-note@+1 {{mark the instance method as 'public' to satisfy the requirement}}
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
  // expected-error@-1{{type 'BadRemoteCall_badResultConformance' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'BadRemoteCall_badResultConformance' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}

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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout BadRemoteCall_badResultConformance.InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res' (aka '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout PublicFakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res')}}
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
  // expected-error@-1{{type 'BadRemoteCall_largeSerializationRequirementSlightlyOffInDefinition' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'BadRemoteCall_largeSerializationRequirementSlightlyOffInDefinition' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-4{{add stubs for conformance}}

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

  // expected-note@+1 {{candidate has non-matching type '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout BadRemoteCall_largeSerializationRequirementSlightlyOffInDefinition.InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res' (aka '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout LargeSerializationReqFakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res')}}
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
  //expected-error@-1{{type 'FakeInvocationEncoder_missing_recordArgument' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  //expected-error@-3{{struct 'FakeInvocationEncoder_missing_recordArgument' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  // MISSING: mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordArgument2: DistributedTargetInvocationEncoder { 
  //expected-error@-1{{type 'FakeInvocationEncoder_missing_recordArgument2' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  //expected-error@-3{{struct 'FakeInvocationEncoder_missing_recordArgument2' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  // expected-note@+1 {{candidate has non-matching type '<Argument> (Argument) throws -> ()'}}
  mutating func recordArgument<Argument: SomeProtocol>(_ argument: Argument) throws {} // BAD
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordReturnType: DistributedTargetInvocationEncoder { 
  //expected-error@-1{{type 'FakeInvocationEncoder_missing_recordReturnType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  //expected-error@-3{{struct 'FakeInvocationEncoder_missing_recordReturnType' is missing witness for protocol requirement 'recordReturnType'}}
  //expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  // MISSING: mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_missing_recordErrorType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{type 'FakeInvocationEncoder_missing_recordErrorType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  // MISSING: mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_recordArgument_wrongType: DistributedTargetInvocationEncoder { 
  //expected-error@-1{{type 'FakeInvocationEncoder_recordArgument_wrongType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  //expected-error@-3{{struct 'FakeInvocationEncoder_recordArgument_wrongType' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: String, other: Argument) throws {} // BAD
  // expected-note@+1 {{candidate has non-matching type '<Argument> (Argument.Type) throws -> ()'}}
  mutating func recordArgument<Argument: SerializationRequirement>(_ argument: Argument.Type) throws {} // BAD
  mutating func recordArgument<Argument: SerializationRequirement>(badName: Argument) throws {} // BAD
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}
struct FakeInvocationEncoder_recordArgument_missingMutating: DistributedTargetInvocationEncoder { 
  //expected-error@-1{{type 'FakeInvocationEncoder_recordArgument_missingMutating' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  //expected-error@-3{{struct 'FakeInvocationEncoder_recordArgument_missingMutating' is missing witness for protocol requirement 'recordArgument'}}
  //expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  // expected-note@+1 {{candidate has non-matching type '<Value> (RemoteCallArgument<Value>) throws -> ()'}}
  func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_recordResultType_wrongType: DistributedTargetInvocationEncoder { 
  //expected-error@-1 {{type 'FakeInvocationEncoder_recordResultType_wrongType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
  //expected-error@-3{{struct 'FakeInvocationEncoder_recordResultType_wrongType' is missing witness for protocol requirement 'recordReturnType'}}
  //expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {}
  mutating func recordReturnType<R: SerializationRequirement>(s: String, _ resultType: R.Type) throws {} // BAD
  // expected-note@+1 {{candidate has non-matching type '<R> (R.Type) throws -> ()'}}
  mutating func recordReturnType<R: SomeProtocol>(_ resultType: R.Type) throws {} // BAD
  mutating func recordReturnType<R: SerializationRequirement>(badName: R.Type) throws {} // BAD
  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  mutating func doneRecording() throws {}
}

struct FakeInvocationEncoder_recordErrorType_wrongType: DistributedTargetInvocationEncoder {
  //expected-error@-1{{type 'FakeInvocationEncoder_recordErrorType_wrongType' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  //expected-note@-2{{add stubs for conformance}}
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
  // expected-error@-2{{method 'decodeNextArgument()' must be declared public because it matches a requirement in public protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-3 {{mark the instance method as 'public' to satisfy the requirement}}
  func decodeReturnType() throws -> Any.Type? { nil }
  // expected-error@-1{{method 'decodeReturnType()' must be declared public because it matches a requirement in public protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-2{{mark the instance method as 'public' to satisfy the requirement}}
  func decodeErrorType() throws -> Any.Type? { nil }
  // expected-error@-1{{method 'decodeErrorType()' must be declared public because it matches a requirement in public protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-2{{mark the instance method as 'public' to satisfy the requirement}}
}

final class PublicFakeInvocationDecoder_badBadProtoRequirement: DistributedTargetInvocationDecoder { 
  // expected-error@-1{{type 'PublicFakeInvocationDecoder_badBadProtoRequirement' does not conform to protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{class 'PublicFakeInvocationDecoder_badBadProtoRequirement' is missing witness for protocol requirement 'decodeNextArgument'}}
  // expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  // expected-note@+1 {{candidate has non-matching type '<Argument> () throws -> Argument'}}
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
  // expected-error@-1{{type 'BadResultHandler_missingOnReturn' does not conform to protocol 'DistributedTargetInvocationResultHandler'}}
  // expected-note@-2{{add stubs for conformance}}{{84-84=\nfunc onReturn<Success: SerializationRequirement>(value: Success) async throws {\n    <#code#>\n\}\n}}
  // expected-error@-3{{struct 'BadResultHandler_missingOnReturn' is missing witness for protocol requirement 'onReturn'}}
  // expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  // func onReturn<Res: SerializationRequirement>(value: Res) async throws {} // MISSING
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}

struct BadResultHandler_missingRequirement: DistributedTargetInvocationResultHandler {
  // expected-error@-1{{struct 'BadResultHandler_missingRequirement' is missing witness for protocol requirement 'onReturn'}}
  // expected-note@-2{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  func onReturn<Success>(value: Success) async throws {} // MISSING : Codable
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}

struct BadResultHandler_mutatingButShouldNotBe: DistributedTargetInvocationResultHandler { 
  // expected-error@-1 {{type 'BadResultHandler_mutatingButShouldNotBe' does not conform to protocol 'DistributedTargetInvocationResultHandler'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{struct 'BadResultHandler_mutatingButShouldNotBe' is missing witness for protocol requirement 'onReturn'}}
  // expected-note@-4{{add stubs for conformance}}
  typealias SerializationRequirement = Codable

  // expected-note@+1 {{candidate is marked 'mutating' but protocol does not allow it}}
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

