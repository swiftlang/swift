// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -I %t
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

import Distributed

final class MyActorSystem {

}

extension MyActorSystem: DistributedActorSystem {
  public typealias ActorID = String
  public typealias InvocationDecoder = DummyInvocationDecoder
  public typealias InvocationEncoder = DummyInvocationEncoder
  public typealias SerializationRequirement = any Codable
  public typealias ResultHandler = DummyResultHandler

  func assignID<Act>(
    _ actorType: Act.Type
  ) -> String
    where
    Act: DistributedActor,
    Act.ID == String
  {
    return "1"
  }

  func resignID(_ id: some Codable) {
    // nothing to do
  }

  func actorReady<Act>(
    _ actor: Act
  )
    where
    Act: DistributedActor,
    Act.ID == String
  {
    // nothing to do
  }

  func resolve<Act>(
    id: some Codable,
    as actorType: Act.Type
  ) throws -> Act?
    where
    Act: DistributedActor,
    Act.ID: Codable
  {
    return nil
  }

  func makeInvocationEncoder() -> DummyInvocationEncoder {
    fatalError("Cannot create an InvocationEncoder")
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
    where
    Act: DistributedActor,
    Act.ID == String,
    Err: Error
  {
    fatalError("Cannot make remote call")
  }

  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
    where
    Act: DistributedActor,
    Act.ID == String,
    Err: Error,
    Res: Codable
  {
    fatalError("Cannot make remote call")
  }
}


// MARK: - DummyInvocationEncoder

struct DummyInvocationEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable

  mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    fatalError("Cannot use DummyInvocationEncoder for distributed targets")
  }

  mutating func recordArgument<Value: SerializationRequirement>(_ argument: RemoteCallArgument<Value>) throws {
    fatalError("Cannot use DummyInvocationEncoder for distributed targets")
  }

  mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    fatalError("Cannot use DummyInvocationEncoder for distributed targets")
  }

  mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    fatalError("Cannot use DummyInvocationEncoder for distributed targets")
  }

  mutating func doneRecording() throws {
    fatalError("Cannot use DummyInvocationEncoder for distributed targets")
  }

  mutating func makeDecoder() -> DummyInvocationDecoder {
    fatalError("Cannot use DummyInvocationEncoder for distributed targets")
  }
}

// MARK: - DummyInvocationDecoder

struct DummyInvocationDecoder: DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable

  func decodeGenericSubstitutions() throws -> [Any.Type] {
    fatalError("Cannot use DummyInvocationDecoder for distributed targets")
  }

  mutating func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError("Cannot use DummyInvocationDecoder for distributed targets")
  }

  func decodeErrorType() throws -> Any.Type? {
    fatalError("Cannot use DummyInvocationDecoder for distributed targets")
  }

  func decodeReturnType() throws -> Any.Type? {
    fatalError("Cannot use DummyInvocationDecoder for distributed targets")
  }
}

// MARK: - DummyResultHandler

struct DummyResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = Codable

  func onReturnVoid() async throws {
    fatalError("Cannot use DummyResultHandler for distributed targets")
  }

  func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    fatalError("Cannot use DummyResultHandler for distributed targets")
  }

  func onThrow<Err>(error: Err) async throws where Err: Error {
    fatalError("Cannot use DummyResultHandler for distributed targets")
  }
}
