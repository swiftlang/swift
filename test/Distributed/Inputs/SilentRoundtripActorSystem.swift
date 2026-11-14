//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0
//
// See LICENSE.txt for license information
// See CONTRIBUTORS.txt for the list of Swift project authors
//
// SPDX-License-Identifier: Apache-2.0
//
//===----------------------------------------------------------------------===//

import Distributed

/// A round-trip actor system that prints nothing: a call on a remote reference
/// is decoded and executed on the local instance through
/// `executeDistributedTarget`, so a test can make many calls and check only
/// their results.
@available(SwiftStdlib 5.7, *)
public final class SilentRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = Int
  public typealias InvocationEncoder = SilentInvocationEncoder
  public typealias InvocationDecoder = SilentInvocationDecoder
  public typealias ResultHandler = SilentResultHandler
  public typealias SerializationRequirement = Codable

  var nextID = 1
  var activeActors: [ActorID: any DistributedActor] = [:]

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor, Act.ID == ActorID {
    nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID {
    defer { nextID += 1 }
    return nextID
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
    activeActors[actor.id] = actor
  }

  public func resignID(_ id: ActorID) {
    activeActors.removeValue(forKey: id)
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  public func remoteCall<Act, Err, Res>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type,
    returning: Res.Type
  ) async throws -> Res
      where Act: DistributedActor, Act.ID == ActorID, Err: Error, Res: SerializationRequirement {
    let result = try await roundtrip(on: actor, target: target, invocation: &invocation)
    return result as! Res
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing: Err.Type
  ) async throws
      where Act: DistributedActor, Act.ID == ActorID, Err: Error {
    _ = try await roundtrip(on: actor, target: target, invocation: &invocation)
  }

  func roundtrip<Act>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder
  ) async throws -> Any?
      where Act: DistributedActor, Act.ID == ActorID {
    guard let active = activeActors[actor.id] else {
      fatalError("No active actor for \(actor.id)")
    }
    let handler = SilentResultHandler()
    var decoder = SilentInvocationDecoder(invocation)

    func execute<A: DistributedActor>(_ active: A) async throws {
      try await executeDistributedTarget(
        on: active,
        target: target,
        invocationDecoder: &decoder,
        handler: handler)
    }
    try await _openExistential(active, do: execute)

    if let error = handler.error {
      throw error
    }
    return handler.value
  }
}

@available(SwiftStdlib 5.7, *)
public struct SilentInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  var genericSubstitutions: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  public init() {}

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    genericSubstitutions.append(type)
  }
  public mutating func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>
  ) throws {
    arguments.append(argument.value)
  }
  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    returnType = type
  }
  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    errorType = type
  }
  public mutating func doneRecording() throws {}
}

@available(SwiftStdlib 5.7, *)
public final class SilentInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  let genericSubstitutions: [Any.Type]
  let arguments: [Any]
  let returnType: Any.Type?
  let errorType: Any.Type?
  var argumentIndex = 0

  public init(_ invocation: SilentInvocationEncoder) {
    self.genericSubstitutions = invocation.genericSubstitutions
    self.arguments = invocation.arguments
    self.returnType = invocation.returnType
    self.errorType = invocation.errorType
  }

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    genericSubstitutions
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    guard argumentIndex < arguments.count else {
      fatalError("Attempted to decode more arguments than recorded: \(argumentIndex), \(arguments)")
    }
    guard let argument = arguments[argumentIndex] as? Argument else {
      fatalError("Cannot cast argument \(arguments[argumentIndex]) to \(Argument.self)")
    }
    argumentIndex += 1
    return argument
  }

  public func decodeErrorType() throws -> Any.Type? {
    errorType
  }

  public func decodeReturnType() throws -> Any.Type? {
    returnType
  }
}

@available(SwiftStdlib 5.7, *)
public final class SilentResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  public var value: Any? = nil
  public var error: Error? = nil

  public init() {}

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    self.value = value
  }
  public func onReturnVoid() async throws {
    self.value = ()
  }
  public func onThrow<Err: Error>(error: Err) async throws {
    self.error = error
  }
}
