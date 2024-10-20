// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

final class SomeClazz: Sendable {}
final class SomeStruct: Sendable {}
final class SomeEnum: Sendable {}

// TODO(distributed): improve to diagnose ON the typealias rather than on the system
// expected-error@+1{{'SerializationRequirement' type witness 'System.SerializationRequirement' (aka 'SomeClazz') must be a protocol type}}
final class System: DistributedActorSystem {
  // ignore those since they all fail with the SerializationRequirement being invalid:
  // expected-error@-2{{type 'System' does not conform to protocol 'DistributedActorSystem'}}
  // expected-note@-3{{add stubs for conformance}}
  // expected-error@-4{{class 'System' is missing witness for protocol requirement 'remoteCall'}}
  // expected-note@-5{{add stubs for conformance}}
  // expected-error@-6{{class 'System' is missing witness for protocol requirement 'remoteCallVoid'}}
  // expected-note@-7{{add stubs for conformance}}
  typealias ActorID = String
  typealias InvocationEncoder = ClassInvocationEncoder
  typealias InvocationDecoder = ClassInvocationDecoder

  typealias ResultHandler = DistributedTargetInvocationResultHandler
  // expected-note@-1{{possibly intended match 'System.ResultHandler' (aka 'DistributedTargetInvocationResultHandler') does not conform to 'DistributedTargetInvocationResultHandler'}}

  typealias SerializationRequirement = SomeClazz

  func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    fatalError()
  }

  func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    fatalError()
  }

  func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
    Act.ID == ActorID {
    fatalError()
  }

  func resignID(_ id: ActorID) {
    fatalError()
  }

  func makeInvocationEncoder() -> InvocationEncoder {
    fatalError()
  }

  // expected-note@+1 {{candidate has non-matching type '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout System.InvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res' (aka '<Act, Err, Res> (on: Act, target: RemoteCallTarget, invocation: inout ClassInvocationEncoder, throwing: Err.Type, returning: Res.Type) async throws -> Res') [with ResultHandler = <<error type>>]}}
  func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type,
    returning returnType: Res.Type
  ) async throws -> Res
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error,
    Res: SerializationRequirement {
    fatalError()
  }

  func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
    Act.ID == ActorID,
    Err: Error {
    fatalError()
  }
}

struct ClassInvocationEncoder: DistributedTargetInvocationEncoder { // expected-error {{type 'ClassInvocationEncoder' does not conform to protocol 'DistributedTargetInvocationEncoder'}}
  // expected-note@-1{{add stubs for conformance}}
  // expected-error@-2{{struct 'ClassInvocationEncoder' is missing witness for protocol requirement 'recordArgument'}}
  // expected-note@-3{{add stubs for conformance}}
  // expected-error@-4{{struct 'ClassInvocationEncoder' is missing witness for protocol requirement 'recordReturnType'}}
  // expected-note@-5{{add stubs for conformance}}
  typealias SerializationRequirement = SomeClazz

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {}
  // expected-note@+1 {{candidate has non-matching type '<Value> (RemoteCallArgument<Value>) throws -> ()'}}
  public mutating func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>) throws {}
  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}
  // expected-note@+1 {{candidate has non-matching type '<R> (R.Type) throws -> ()'}}
  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}
  public mutating func doneRecording() throws {}
}

final class ClassInvocationDecoder: DistributedTargetInvocationDecoder { // expected-error {{type 'ClassInvocationDecoder' does not conform to protocol 'DistributedTargetInvocationDecoder'}}
  // expected-note@-1{{add stubs for conformance}}
  // expected-error@-2{{class 'ClassInvocationDecoder' is missing witness for protocol requirement 'decodeNextArgument'}}
  // expected-note@-3{{add stubs for conformance}}
  typealias SerializationRequirement = SomeClazz

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    fatalError()
  }

  // expected-note@+1 {{candidate has non-matching type '<Argument> () throws -> Argument'}}
  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    fatalError()
  }

  public func decodeErrorType() throws -> Any.Type? {
    fatalError()
  }

  public func decodeReturnType() throws -> Any.Type? {
    fatalError()
  }
}

struct DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = SomeClazz
  func onReturn<Success: SomeClazz>(value: Success) async throws {}
  func onReturnVoid() async throws {}
  func onThrow<Err: Error>(error: Err) async throws {}
}
