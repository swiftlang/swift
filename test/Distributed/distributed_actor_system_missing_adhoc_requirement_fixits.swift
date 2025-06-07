// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

struct MissingRemoteCall: DistributedActorSystem { 
  // expected-error@-1{{struct 'MissingRemoteCall' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{add stubs for conformance}}{{51-51=\nfunc remoteCall<Act, Err, Res>(on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res where Act: DistributedActor, Act.ID == ActorID, Err: Error, Res: SerializationRequirement {\n    <#code#>\n}\n}}
  // expected-error@-3{{struct 'MissingRemoteCall' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-4{{add stubs for conformance}}{{51-51=\nfunc remoteCallVoid<Act, Err>(on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder, throwing: Err.Type) async throws where Act: DistributedActor, Act.ID == ActorID, Err: Error {\n    <#code#>\n}\n}}
  // expected-error@-5 {{type 'MissingRemoteCall' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-6{{add stubs for conformance}} {{51-51=\n    func remoteCall<Act, Err, Res>(on actor: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res where Act : DistributedActor, Err : Error, ActorAddress == Act.ID {\n        <#code#>\n    \}\n\n    func remoteCallVoid<Act, Err>(on actor: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type) async throws where Act : DistributedActor, Err : Error, ActorAddress == Act.ID {\n        <#code#>\n    \}\n}}
  

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

public struct PublicMissingRemoteCall: DistributedActorSystem { 
  // expected-error@-1{{struct 'PublicMissingRemoteCall' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-2{{add stubs for conformance}}{{64-64=\npublic func remoteCall<Act, Err, Res>(on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res where Act: DistributedActor, Act.ID == ActorID, Err: Error, Res: SerializationRequirement {\n    <#code#>\n}\n}}
  // expected-error@-3{{struct 'PublicMissingRemoteCall' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-4{{add stubs for conformance}}{{64-64=\npublic func remoteCallVoid<Act, Err>(on actor: Act, target: RemoteCallTarget, invocation: inout InvocationEncoder, throwing: Err.Type) async throws where Act: DistributedActor, Act.ID == ActorID, Err: Error {\n    <#code#>\n}\n}}
  // expected-error@-5{{type 'PublicMissingRemoteCall' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-6{{add stubs for conformance}}{{64-64=\n    public func remoteCall<Act, Err, Res>(on actor: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res where Act : DistributedActor, Err : Error, ActorAddress == Act.ID {\n        <#code#>\n    \}\n\n    public func remoteCallVoid<Act, Err>(on actor: Act, target: RemoteCallTarget, invocation: inout FakeInvocationEncoder, throwing: Err.Type) async throws where Act : DistributedActor, Err : Error, ActorAddress == Act.ID {\n        <#code#>\n    \}\n}}
  


  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
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
    Act.ID == ActorID {
  }

  public func resignID(_ id: ActorID) {
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
  }
}

// ==== ------------------------------------------------------------------------

public struct ActorAddress: Sendable, Hashable, Codable {
  let address: String

  init(parse address: String) {
    self.address = address
  }
}

public protocol SomeProtocol: Sendable {
}

public struct FakeInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
  }

  public mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {
  }

  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
  }

  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
  }

  public mutating func doneRecording() throws {
  }
}

public class FakeInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    []
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError()
  }

  public func decodeReturnType() throws -> Any.Type? {
    nil
  }

  public func decodeErrorType() throws -> Any.Type? {
    nil
  }
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


