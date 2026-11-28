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

/// A fake actor system whose proxies can carry per-proxy call options.

import Distributed
import FakeCodableForDistributedTests

// ==== ------------------------------------------------------------------------
// MARK: Actor ID

@available(SwiftStdlib 5.7, *)
public struct FakeOptionsActorID: Hashable, Sendable, Codable, CustomStringConvertible {
  /// The node hosting the actor.
  public let node: String
  /// The actor's name, unique within its node.
  public let name: String

  /// Call options of the proxy holding this ID.
  ///
  /// Local to this process: not encoded, and not part of `==` or `hash`, so a
  /// proxy derived with options finds the same route as the proxy it came from.
  public internal(set) var callOptions: FakeCallOptions? = nil

  public init(node: String, name: String) {
    self.node = node
    self.name = name
  }

  public static func == (lhs: Self, rhs: Self) -> Bool {
    lhs.node == rhs.node && lhs.name == rhs.name
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(node)
    hasher.combine(name)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode("\(node)/\(name)")
  }

  public init(from decoder: Decoder) throws {
    let string = try decoder.singleValueContainer().decode(String.self)
    let parts = string.split(separator: "/", maxSplits: 1)
    guard parts.count == 2 else {
      throw FakeOptionsActorSystemError.malformedID(string)
    }
    self.init(node: String(parts[0]), name: String(parts[1]))
  }

  public var description: String {
    "\(node)/\(name)"
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Deriving a proxy with call options

@available(SwiftStdlib 5.7, *)
extension DistributedActor where ActorSystem == FakeOptionsActorSystem {

  /// A reference to the same remote actor, whose calls use these options.
  ///
  /// Starts from this reference's options, if any; `self` is unchanged. A local
  /// actor is returned as is, since its calls never reach `remoteCall`.
  public nonisolated func withCallOptions(
    _ update: (inout FakeCallOptions) -> Void
  ) -> Self {
    guard __isRemoteActor(self) else {
      return self
    }

    var id = self.id
    var options = id.callOptions ?? FakeCallOptions()
    update(&options)
    id.callOptions = options

    do {
      return try Self.resolve(id: id, using: actorSystem)
    } catch {
      // Unreachable: `id` is equal to the ID of a live remote proxy of type
      // `Self`, so `resolve(id:as:)` finds no local actor and the runtime
      // creates a proxy.
      fatalError("Failed to derive \(Self.self) proxy for \(id): \(error)")
    }
  }

  /// This reference's call options, or `nil` if it has none.
  public nonisolated var callOptions: FakeCallOptions? {
    id.callOptions
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Connections

/// A link from one node to another; `reverse` is the link back.
@available(SwiftStdlib 5.7, *)
public final class FakeConnection: @unchecked Sendable, CustomStringConvertible {
  public unowned let from: FakeOptionsActorSystem
  public unowned let to: FakeOptionsActorSystem
  public fileprivate(set) weak var reverse: FakeConnection?

  /// How many of the next connection attempts fail; each attempt uses up one.
  public var failingAttempts: Int = 0

  fileprivate init(from: FakeOptionsActorSystem, to: FakeOptionsActorSystem) {
    self.from = from
    self.to = to
  }

  public var description: String {
    "\(from.node)->\(to.node)"
  }
}

@available(SwiftStdlib 5.7, *)
public enum FakeOptionsActorSystemError: Error, CustomStringConvertible {
  case noConnection(FakeOptionsActorID)
  case connectionFailed(String, attempts: Int)
  case unknownRecipient(FakeOptionsActorID, node: String)
  case wrongActorType(FakeOptionsActorID)
  case malformedID(String)
  case genericsNotSupported

  public var description: String {
    switch self {
    case .noConnection(let id): return "no connection for \(id)"
    case .connectionFailed(let node, let attempts):
      return "connection to \(node) failed after \(attempts) attempt\(attempts == 1 ? "" : "s")"
    case .unknownRecipient(let id, let node): return "\(node) has no actor \(id)"
    case .wrongActorType(let id): return "\(id) has a different type"
    case .malformedID(let string): return "malformed ID '\(string)'"
    case .genericsNotSupported: return "generic distributed funcs are not supported"
    }
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Actor system

@available(SwiftStdlib 5.7, *)
public final class FakeOptionsActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = FakeOptionsActorID
  public typealias InvocationEncoder = FakeOptionsInvocationEncoder
  public typealias InvocationDecoder = FakeOptionsInvocationDecoder
  public typealias ResultHandler = FakeOptionsResultHandler
  public typealias SerializationRequirement = Codable

  /// The connection executing the remote call this task is part of: the
  /// inbound connection while handling a call, the outbound one while
  /// decoding a reply.
  // A plain `TaskLocal` rather than the `@TaskLocal` macro, so the test does
  // not need the macro plugin.
  public static let connectionExecutingRemoteCall = TaskLocal<FakeConnection?>(wrappedValue: nil)

  public let node: String

  /// Whether `resolve(id:as:)` leaves the route of an ID that carries call
  /// options alone.
  ///
  /// When `false`, an ID resolved while a remote call executes is routed over
  /// that call's connection, even when it was derived from a proxy that already
  /// has a route; this re-routes the original proxy too.
  public var protectsDerivedIDs: Bool = true

  private var localActors: [ActorID: any DistributedActor] = [:]
  private var routes: [ActorID: FakeConnection] = [:]
  private var connections: [FakeConnection] = []
  private var nextActorNumber = 0
  private var nextCallID = 0

  public init(node: String) {
    self.node = node
  }

  /// Links this node and `peer`, and returns the connection to `peer`.
  public func connect(to peer: FakeOptionsActorSystem) -> FakeConnection {
    let there = FakeConnection(from: self, to: peer)
    let back = FakeConnection(from: peer, to: self)
    there.reverse = back
    back.reverse = there
    self.connections.append(there)
    peer.connections.append(back)
    return there
  }

  /// A proxy for `id`, routed over `connection`; what a session vending a
  /// proxy does.
  public func makeProxy<Act>(
    _ actorType: Act.Type,
    for id: ActorID,
    over connection: FakeConnection
  ) throws -> Act
    where Act: DistributedActor, Act.ActorSystem == FakeOptionsActorSystem {
    routes[id] = connection
    return try Act.resolve(id: id, using: self)
  }

  /// The connection calls to `id` currently go over.
  public func route(for id: ActorID) -> String {
    routes[id].map { "\($0)" } ?? "none"
  }

  // ==== ----------------------------------------------------------------------
  // MARK: Lifecycle

  public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
    where Act: DistributedActor, Act.ID == ActorID {
    if let local = localActors[id] {
      guard let typed = local as? Act else {
        throw FakeOptionsActorSystemError.wrongActorType(id)
      }
      return typed
    }

    // An unknown ID arriving during a remote call belongs to that call's peer.
    if let connection = Self.connectionExecutingRemoteCall.get(),
       connection.from === self {
      if protectsDerivedIDs && id.callOptions != nil {
        print("| \(node): resolve \(id) with options, keep route \(route(for: id))")
      } else {
        print("| \(node): resolve \(id), route over \(connection)")
        routes[id] = connection
      }
    }
    return nil
  }

  public func assignID<Act>(_ actorType: Act.Type) -> ActorID
    where Act: DistributedActor, Act.ID == ActorID {
    defer { nextActorNumber += 1 }
    return ActorID(node: node, name: "\(Act.self)-\(nextActorNumber)")
  }

  public func actorReady<Act>(_ actor: Act)
    where Act: DistributedActor, Act.ID == ActorID {
    localActors[actor.id] = actor
  }

  public func resignID(_ id: ActorID) {
    localActors[id] = nil
  }

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
  }

  // ==== ----------------------------------------------------------------------
  // MARK: Outbound calls

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
    let (connection, reply) = try await send(
      to: actor.id, target: target, arguments: invocation.arguments)
    // Actors in the reply belong to the peer that sent it.
    return try Self.connectionExecutingRemoteCall.withValue(connection) {
      let decoder = FakeDecoder()
      decoder.userInfo[.actorSystemKey] = self
      return try decoder.decode(reply!, as: Res.self)
    }
  }

  public func remoteCallVoid<Act, Err>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder,
    throwing errorType: Err.Type
  ) async throws
    where Act: DistributedActor,
          Act.ID == ActorID,
          Err: Error {
    _ = try await send(
      to: actor.id, target: target, arguments: invocation.arguments)
  }

  /// Applies the recipient's call options, then delivers the call to the
  /// peer node. Returns the connection used and the encoded reply.
  private func send(
    to id: ActorID,
    target: RemoteCallTarget,
    arguments: [String]
  ) async throws -> (FakeConnection, String?) {
    let callID = nextCallID
    nextCallID += 1
    let options = id.callOptions
    func report(_ kind: FakeCallEvent.Kind) {
      options?.onEvent?(FakeCallEvent(callID: callID, kind: kind))
    }

    guard let connection = routes[id] else {
      throw FakeOptionsActorSystemError.noConnection(id)
    }
    print("  >> \(node): call #\(callID) \(target) on \(id) over \(connection), options: \(options?.description ?? "none")")

    // The call has not been sent yet, so a failed attempt is safe to retry.
    let retries = options?.automaticRetries ?? 0
    var attempt = 1
    while connection.failingAttempts > 0 {
      connection.failingAttempts -= 1
      print("     \(node): connection attempt \(attempt) to \(connection.to.node) failed")
      guard attempt <= retries else {
        throw FakeOptionsActorSystemError.connectionFailed(
          connection.to.node, attempts: attempt)
      }
      attempt += 1
      report(.retrying(attempt: attempt))
    }

    // Only the encoded ID goes on the wire; the options stay here.
    let wireRecipient = try FakeEncoder().encode(id)
    print("     wire: recipient \(wireRecipient) arguments \(arguments)")
    report(.sent)
    defer { report(.finished) }

    let reply = try await connection.to.receive(
      recipient: wireRecipient,
      target: target,
      arguments: arguments,
      over: connection.reverse!)
    return (connection, reply)
  }

  // ==== ----------------------------------------------------------------------
  // MARK: Inbound calls

  private func receive(
    recipient wireRecipient: String,
    target: RemoteCallTarget,
    arguments: [String],
    over connection: FakeConnection
  ) async throws -> String? {
    let id = try FakeDecoder().decode(wireRecipient, as: ActorID.self)
    guard let actor = localActors[id] else {
      throw FakeOptionsActorSystemError.unknownRecipient(id, node: node)
    }
    print("  << \(node): execute \(target) on \(id), from \(connection.to.node)")

    let reply = FakeOptionsReplyBox()
    var decoder = FakeOptionsInvocationDecoder(system: self, arguments: arguments)
    try await Self.connectionExecutingRemoteCall.withValue(connection) {
      try await executeDistributedTarget(
        on: actor,
        target: target,
        invocationDecoder: &decoder,
        handler: FakeOptionsResultHandler(reply: reply))
    }

    if let error = reply.error {
      throw error
    }
    return reply.value
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Invocation encoding

@available(SwiftStdlib 5.7, *)
public struct FakeOptionsInvocationEncoder: DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  /// Each argument, encoded with `FakeEncoder`.
  public private(set) var arguments: [String] = []

  public init() {}

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    throw FakeOptionsActorSystemError.genericsNotSupported
  }

  public mutating func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>
  ) throws {
    arguments.append(try FakeEncoder().encode(argument.value))
  }

  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {}

  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {}

  public mutating func doneRecording() throws {}
}

@available(SwiftStdlib 5.7, *)
public final class FakeOptionsInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  let system: FakeOptionsActorSystem
  let arguments: [String]
  var argumentIndex = 0

  init(system: FakeOptionsActorSystem, arguments: [String]) {
    self.system = system
    self.arguments = arguments
  }

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    []
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    defer { argumentIndex += 1 }
    let decoder = FakeDecoder()
    decoder.userInfo[.actorSystemKey] = system
    return try decoder.decode(arguments[argumentIndex], as: Argument.self)
  }

  public func decodeErrorType() throws -> Any.Type? {
    nil
  }

  public func decodeReturnType() throws -> Any.Type? {
    nil
  }
}

@available(SwiftStdlib 5.7, *)
final class FakeOptionsReplyBox: @unchecked Sendable {
  var value: String? = nil
  var error: (any Error)? = nil
}

@available(SwiftStdlib 5.7, *)
public struct FakeOptionsResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  let reply: FakeOptionsReplyBox

  init(reply: FakeOptionsReplyBox) {
    self.reply = reply
  }

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    reply.value = try FakeEncoder().encode(value)
  }

  public func onReturnVoid() async throws {}

  public func onThrow<Err: Error>(error: Err) async throws {
    // Errors are handed over as they are; the fake does not encode them.
    reply.error = error
  }
}
