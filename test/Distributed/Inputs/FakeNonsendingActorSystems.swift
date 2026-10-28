//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0
//
// See LICENSE.txt for license information
// See CONTRIBUTORS.txt for the list of Swift project authors
//
// SPDX-License-Identifier: Apache-2.0
//
//===----------------------------------------------------------------------===//

// Actor systems whose ad-hoc `remoteCall` requirements are declared
// 'nonisolated(nonsending)'.

import Distributed
import FakeDistributedActorSystems

// ==== -----------------------------------------------------------------------
// MARK: 'nonisolated(nonsending)' actor systems

@available(SwiftStdlib 6.0, *)
public struct FakeNonsendingActorSystem: DistributedActorSystem, CustomStringConvertible {
  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  public init() {}

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor,
            Act.ID == ActorID {
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

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  nonisolated(nonsending)
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
          Res: SerializationRequirement {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }

  nonisolated(nonsending)
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

  public nonisolated var description: Swift.String {
    "\(Self.self)()"
  }
}

/// Round-tripping 'nonisolated(nonsending)' system, mirroring
/// `FakeRoundtripActorSystem`: `resolve` always reports remote, and
/// `remoteCall` loops the invocation back into `executeDistributedTarget` on
/// the local instance. That drives the recipient-side distributed accessor,
/// which for a 'nonisolated(nonsending)' thunk has to supply the implicit
/// leading actor parameter itself.
@available(SwiftStdlib 6.0, *)
public final class FakeNonsendingRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = ActorAddress
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  var activeActors: [ActorID: any DistributedActor] = [:]

  /// Invoked synchronously at the top of `remoteCall` / `remoteCallVoid`,
  /// before anything is awaited, passing the `#isolation` seen there. Because
  /// these witnesses are 'nonisolated(nonsending)' that is the caller's
  /// isolation, so a test can assert it was forwarded rather than hopped away
  /// from. Assert on the executor as well as the value: checking `#isolation`
  /// alone can pass while actually running on the target actor's executor
  public var onRemoteCall: (@Sendable ((any Actor)?) -> Void)? = nil

  /// When true, `resolve` hands back the locally registered instance instead of
  /// reporting remote, which makes a distributed thunk take its *local* branch
  public var resolveLocally: Bool = false

  /// When set, `remoteCall` returns this value and `remoteCallVoid` simply
  /// returns, in both cases *without* executing the target. That leaves only
  /// the sending side of the call, so a test can assert which executor the
  /// caller ends up on without a recipient-side hop confusing the picture
  public var stubbedRemoteCallReply: Any? = nil

  public init() {}

  public func shutdown() {
    self.activeActors = [:]
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    if resolveLocally, let active = activeActors[id] as? Act {
      print("| resolve \(id) as local")
      return active
    }
    print("| resolve \(id) as remote")
    return nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor {
    let id = ActorAddress(parse: "<unique-id>")
    print("| assign id: \(id) for \(actorType)")
    return id
  }

  public func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor,
          Act.ID == ActorID {
    print("| actor ready: \(actor)")
    self.activeActors[actor.id] = actor
  }

  public func resignID(_ id: ActorID) {
    print("X resign id: \(id)")
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  private var remoteCallResult: Any? = nil
  private var remoteCallError: Error? = nil

  nonisolated(nonsending)
  public func remoteCall<Act, Err, Res>(
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
    onRemoteCall?(#isolation)
    print("  >> remoteCall: on:\(actor), target:\(target), throwing:\(String(reflecting: errorType)), returning:\(String(reflecting: returnType))")
    if let stubbedRemoteCallReply {
      print("  << remoteCall stubbed reply: \(stubbedRemoteCallReply)")
      return stubbedRemoteCallReply as! Res
    }
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor")
    }

    func doIt<A: DistributedActor>(active: A) async throws -> Res {
      let resultHandler = FakeRoundtripResultHandler { value in
        self.remoteCallResult = value
        self.remoteCallError = nil
      } onError: { error in
        self.remoteCallResult = nil
        self.remoteCallError = error
      }

      var decoder = invocation.makeDecoder()

      print(" > execute distributed target: \(target)")
      try await executeDistributedTarget(
        on: active,
        target: target,
        invocationDecoder: &decoder,
        handler: resultHandler
      )

      switch (remoteCallResult, remoteCallError) {
      case (.some(let value), nil):
        print("  << remoteCall return: \(value)")
        return remoteCallResult! as! Res
      case (nil, .some(let error)):
        print("  << remoteCall throw: \(error)")
        throw error
      default:
        fatalError("No reply!")
      }
    }
    return try await _openExistential(targetActor, do: doIt)
  }

  nonisolated(nonsending)
  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    onRemoteCall?(#isolation)
    print("  >> remoteCallVoid: on:\(actor), target:\(target), throwing:\(String(reflecting: errorType))")
    if stubbedRemoteCallReply != nil {
      print("  << remoteCallVoid stubbed")
      return
    }
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor")
    }

    func doIt<A: DistributedActor>(active: A) async throws {
      let resultHandler = FakeRoundtripResultHandler { value in
        self.remoteCallResult = value
        self.remoteCallError = nil
      } onError: { error in
        self.remoteCallResult = nil
        self.remoteCallError = error
      }

      var decoder = invocation.makeDecoder()

      print(" > execute distributed target: \(target)")
      try await executeDistributedTarget(
        on: active,
        target: target,
        invocationDecoder: &decoder,
        handler: resultHandler
      )

      switch (remoteCallResult, remoteCallError) {
      case (.some, nil):
        return
      case (nil, .some(let error)):
        print("  << remoteCall throw: \(error)")
        throw error
      default:
        fatalError("No reply!")
      }
    }
    try await _openExistential(targetActor, do: doIt)
  }
}
