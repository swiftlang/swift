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

@_spi(OnewaySend) import Distributed

// ==== Example Distributed Actors ----------------------------------------------

@available(SwiftStdlib 5.7, *)
public distributed actor FakeRoundtripActorSystemDistributedActor {
  public typealias ActorSystem = FakeRoundtripActorSystem
}


@available(SwiftStdlib 5.7, *)
@_transparent
public func takeIsolatedDistributedActorReturnAsLocalActor<T: DistributedActor>(_ t: isolated T) -> any Actor {
  return t.asLocalActor
}

// ==== Fake Address -----------------------------------------------------------

public struct ActorAddress: Hashable, Sendable, Codable {
  public let address: String

  public init(parse address: String) {
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

/// This type is same as ActorAddress however for purposes of SIL tests we make it not-loadable.
///
/// By adding the `Any` we don't know the full size of the struct making the type in SIL `$*ActorAddress`
/// when we try to store or pass it around, which triggers `isAddressOnly` guarded paths which we need to test.
public struct NotLoadableActorAddress: Hashable, Sendable, Codable {
  public let address: String
  public let any: Sendable = "" // DO NOT REMOVE, this makes this struct address-only which is crucial for testing.

  public init(parse address: String) {
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

  public func hash(into hasher: inout Swift.Hasher) {
  }

  public static func ==(lhs: NotLoadableActorAddress, rhs: NotLoadableActorAddress) -> Bool {
    lhs.address == rhs.address
  }
}

// ==== Noop Transport ---------------------------------------------------------

@available(SwiftStdlib 5.7, *)
public struct FakeActorSystem: DistributedActorSystem, CustomStringConvertible {
  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  // just so that the struct does not become "trivial"
  let someValue: String = ""
  let someValue2: String = ""
  let someValue3: String = ""
  let someValue4: String = ""

  public init() {
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

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
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
          Res: SerializationRequirement {
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

  public nonisolated var description: Swift.String {
    "\(Self.self)()"
  }
}

@_spi(OnewaySend)
@available(SwiftStdlib 6.5, *)
extension FakeActorSystem: _DistributedOnewayActorSystem {
  public func remoteCallOneway<Act>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation invocationEncoder: inout InvocationEncoder
  ) throws
    where Act: DistributedActor,
          Act.ID == ActorID {
    throw ExecuteDistributedTargetError(message: "\(#function) not implemented.")
  }
}

@available(SwiftStdlib 5.7, *)
//@_spi(FakeDistributedActorSystems)
public struct FakeActorSystemWithSPI: DistributedActorSystem, CustomStringConvertible {
  public typealias ActorID = ActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  // just so that the struct does not become "trivial"
  let someValue: String = ""
  let someValue2: String = ""
  let someValue3: String = ""
  let someValue4: String = ""

  public init() {
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

  public func makeInvocationEncoder() -> InvocationEncoder {
    .init()
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
          Res: SerializationRequirement {
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

  public nonisolated var description: Swift.String {
    "\(Self.self)()"
  }
}

@available(SwiftStdlib 5.7, *)
public struct FakeNotLoadableAddressActorSystem: DistributedActorSystem, CustomStringConvertible {
  public typealias ActorID = NotLoadableActorAddress
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  // just so that the struct does not become "trivial"
  let someValue: String = ""
  let someValue2: String = ""
  let someValue3: String = ""
  let someValue4: String = ""

  public init() {
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
    NotLoadableActorAddress(parse: "xxx")
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

// ==== Fake Roundtrip Transport -----------------------------------------------

@available(SwiftStdlib 5.7, *)
/// The reply of one remote call of a roundtrip actor system, so that
/// concurrent calls, like the '@oneway' calls made with 'send', do not
/// overwrite each other's replies
final class FakeRoundtripReply: @unchecked Sendable {
  var result: Any? = nil
  var error: (any Error)? = nil
}

public final class FakeRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = ActorAddress
  public typealias InvocationEncoder = FakeInvocationEncoder
  public typealias InvocationDecoder = FakeInvocationDecoder
  public typealias SerializationRequirement = Codable
  public typealias ResultHandler = FakeRoundtripResultHandler

  var activeActors: [ActorID: any DistributedActor] = [:]
  var forcedNextRemoteCallReply: Any? = nil

  // Delivers the '@oneway' calls, see 'remoteCallOneway'
  let onewayTransport = FakeOnewayTransport()

  public init() {}

  public func shutdown() {
    self.activeActors = [:]
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    print("| resolve \(id) as remote // this system always resolves as remote")
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


  public func forceNextRemoteCallReply(_ reply: Any) {
    self.forcedNextRemoteCallReply = reply
  }

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
    print("  >> remoteCall: on:\(actor), target:\(target), invocation:\(invocation), throwing:\(String(reflecting: errorType)), returning:\(String(reflecting: returnType))")
    print(" > execute distributed target: \(target), identifier: \(target.identifier)")
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor: \(target.identifier)")
    }

    if let forcedNextRemoteCallReply {
      defer { self.forcedNextRemoteCallReply = nil }
      return forcedNextRemoteCallReply as! Res
    }

    func doIt<A: DistributedActor>(active: A) async throws -> Res {
      guard (actor.id) == active.id as! ActorID else {
        fatalError("Attempted to call mock 'roundtrip' on unknown actor: \(actor.id), known: \(active.id)")
      }

      let reply = FakeRoundtripReply()
      let resultHandler = FakeRoundtripResultHandler { value in
        reply.result = value
        reply.error = nil
      } onError: { error in
        reply.result = nil
        reply.error = error
      }

      // The envelope which is sent to the recipient
      let envelope = FakeTargetEnvelope(target)
      var decoder = invocation.makeDecoder()

      let recipientTarget = fakeRecipientTarget(envelope)
      try await executeDistributedTarget(
        on: active,
        target: recipientTarget,
        invocationDecoder: &decoder,
        handler: resultHandler
      )

      switch (reply.result, reply.error) {
      case (.some(let value), nil):
        print("  << remoteCall return: \(value)")
        return reply.result! as! Res
      case (nil, .some(let error)):
        print("  << remoteCall throw: \(error)")
        throw error
      default:
        fatalError("No reply!")
      }
    }
    return try await _openExistential(targetActor, do: doIt)
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
    print("  >> remoteCallVoid: on:\(actor), target:\(target), invocation:\(invocation), throwing:\(String(reflecting: errorType))")
    if #available(SwiftStdlib 6.5, *), target.isOnewayRemoteCall {
      print("  >> remoteCallVoid: is oneway call")
    }
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor")
    }

    func doIt<A: DistributedActor>(active: A) async throws {
      guard (actor.id) == active.id as! ActorID else {
        fatalError("Attempted to call mock 'roundtrip' on unknown actor: \(actor.id), known: \(active.id)")
      }

      let reply = FakeRoundtripReply()
      let resultHandler = FakeRoundtripResultHandler { value in
        reply.result = value
        reply.error = nil
      } onError: { error in
        reply.result = nil
        reply.error = error
      }

      // The envelope which is sent to the recipient
      let envelope = FakeTargetEnvelope(target)
      var decoder = invocation.makeDecoder()

      print(" > execute distributed target: \(target)")
      let recipientTarget = fakeRecipientTarget(envelope)
      try await executeDistributedTarget(
        on: active,
        target: recipientTarget,
        invocationDecoder: &decoder,
        handler: resultHandler
      )

      switch (reply.result, reply.error) {
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

@_spi(OnewaySend)
@available(SwiftStdlib 6.5, *)
extension FakeRoundtripActorSystem: _DistributedOnewayActorSystem {
  /// Hands the call to the fake transport and returns right away, the
  /// transport executes the calls on the "remote" actor in the order they
  /// were sent, see 'FakeOnewayTransport'
  public func remoteCallOneway<Act>(
    on actor: Act,
    target: RemoteCallTarget,
    invocation: inout InvocationEncoder
  ) throws
    where Act: DistributedActor,
          Act.ID == ActorID {
    print("  >> remoteCallOneway: on:\(actor), target:\(target), invocation:\(invocation), oneway: \(target.isOnewayRemoteCall), onewayTarget: \(target.isOnewayTarget)")
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor")
    }

    let message = FakeOnewayMessage(
      system: self, actor: targetActor, target: FakeTargetEnvelope(target),
      decoder: invocation.makeDecoder())
    // Runs synchronously up to the hop to the transport, so the calls reach
    // the transport in the order they were sent
    let transport = self.onewayTransport
    Task.immediate {
      await transport.send(message)
    }
  }
}

/// A '@oneway' call on its way to the "remote" actor
@available(SwiftStdlib 5.7, *)
struct FakeOnewayMessage: @unchecked Sendable {
  let system: FakeRoundtripActorSystem
  let actor: any DistributedActor
  let target: FakeTargetEnvelope
  let decoder: FakeInvocationDecoder
}

/// The transport of the '@oneway' calls of 'FakeRoundtripActorSystem'. It
/// executes the calls one after the other, in the order they were sent, and
/// nobody waits for them
@available(SwiftStdlib 5.7, *)
actor FakeOnewayTransport {
  var outbox: [FakeOnewayMessage] = []
  var isDelivering = false

  func send(_ message: FakeOnewayMessage) {
    outbox.append(message)
    if !isDelivering {
      isDelivering = true
      Task { await self.deliverAll() }
    }
  }

  func deliverAll() async {
    while !outbox.isEmpty {
      let message = outbox.removeFirst()
      await Self.deliver(message)
    }
    isDelivering = false
  }

  nonisolated static func deliver(_ message: FakeOnewayMessage) async {
    func doIt<A: DistributedActor>(active: A) async {
      let resultHandler = FakeRoundtripResultHandler { _ in
      } onError: { error in
        print("  << remoteCallOneway error: \(error)")
      }
      var decoder = message.decoder
      print(" > execute oneway distributed target: \(message.target.identifier)")
      let recipientTarget = fakeRecipientTarget(message.target)
      do {
        try await message.system.executeDistributedTarget(
          on: active,
          target: recipientTarget,
          invocationDecoder: &decoder,
          handler: resultHandler
        )
      } catch {
        print("  << remoteCallOneway execute error: \(error)")
      }
    }
    await _openExistential(message.actor, do: doIt)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Carrying the oneway flags of a RemoteCallTarget over the "wire"

/// What the sender puts into the envelope next to the invocation: the target
/// identifier and the oneway flags of the 'RemoteCallTarget'
@available(SwiftStdlib 5.7, *)
struct FakeTargetEnvelope: Sendable {
  let identifier: String
  var isOnewayRemoteCall: Bool = false
  var isOnewayTarget: Bool = false

  init(_ target: RemoteCallTarget) {
    self.identifier = target.identifier
    if #available(SwiftStdlib 6.5, *) {
      self.isOnewayRemoteCall = target.isOnewayRemoteCall
      self.isOnewayTarget = target.isOnewayTarget
    }
  }
}

/// Tests which check the oneway flags the recipient sees turn this on. It is
/// off by default so that the many tests which share these systems, and
/// match their output line by line, don't see the extra line
@available(SwiftStdlib 5.7, *)
public enum FakeRecipientTargetPrinting {
  nonisolated(unsafe) public static var isEnabled = false
}

/// The recipient builds a fresh target from the identifier, and restores the
/// oneway flags which were sent along with the invocation
@available(SwiftStdlib 5.7, *)
func fakeRecipientTarget(_ envelope: FakeTargetEnvelope) -> RemoteCallTarget {
  var target = RemoteCallTarget(envelope.identifier)
  if #available(SwiftStdlib 6.5, *) {
    target.isOnewayTarget = envelope.isOnewayTarget
    if envelope.isOnewayRemoteCall { target._markOnewayRemoteCall() }
    if FakeRecipientTargetPrinting.isEnabled {
      print("  >> executeDistributedTarget: \(target.identifier) oneway: \(target.isOnewayRemoteCall) target: \(target.isOnewayTarget)")
    }
  }
  return target
}

@available(SwiftStdlib 5.7, *)
public struct FakeInvocationEncoder : DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = Codable

  var genericSubs: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  public init() {}

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    print(" > encode generic sub: \(String(reflecting: type))")
    genericSubs.append(type)
  }

  public mutating func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>) throws {
    print(" > encode argument name:\(argument.label ?? "_"), value: \(argument.value)")
    arguments.append(argument.value)
  }

  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    print(" > encode error type: \(String(reflecting: type))")
    self.errorType = type
  }

  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    print(" > encode return type: \(String(reflecting: type))")
    self.returnType = type
  }

  public mutating func doneRecording() throws {
    print(" > done recording")
  }

  public mutating func makeDecoder() -> FakeInvocationDecoder {
    defer {
      // reset the decoder; we don't want to keep these values retained by accident here
      genericSubs = []
      arguments = []
      returnType = nil
      errorType = nil
    }
    return .init(
      args: arguments,
      substitutions: genericSubs,
      returnType: returnType,
      errorType: errorType
    )
  }
}

// === decoding --------------------------------------------------------------

// !!! WARNING !!!
// This is a 'final class' on purpose, to see that we retain the ad-hoc witness
// for 'decodeNextArgument'; Do not change it to just a class!
@available(SwiftStdlib 5.7, *)
public final class FakeInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = Codable

  var genericSubs: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  var argumentIndex: Int = 0

  public init(
    args: [Any],
    substitutions: [Any.Type] = [],
    returnType: Any.Type? = nil,
    errorType: Any.Type? = nil
  ) {
    self.arguments = args
    self.genericSubs = substitutions
    self.returnType = returnType
    self.errorType = errorType
  }

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    print("  > decode generic subs: \(genericSubs)")
    return genericSubs
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    guard argumentIndex < arguments.count else {
      fatalError("Attempted to decode more arguments than stored! Index: \(argumentIndex), args: \(arguments)")
    }

    let anyArgument = arguments[argumentIndex]
    guard let argument = anyArgument as? Argument else {
      fatalError("Cannot cast argument\(anyArgument) to expected \(Argument.self)")
    }

    print("  > decode argument: \(argument)")
    argumentIndex += 1
    return argument
  }

  public func decodeErrorType() throws -> Any.Type? {
    print("  > decode return type: \(errorType.map { String(reflecting: $0) }  ?? "nil")")
    return self.errorType
  }

  public func decodeReturnType() throws -> Any.Type? {
    print("  > decode return type: \(returnType.map { String(reflecting: $0) }  ?? "nil")")
    return self.returnType
  }
}

@available(SwiftStdlib 5.7, *)
public struct FakeRoundtripResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = Codable

  let storeReturn: (any Any) -> Void
  let storeError: (any Error) -> Void
  public init(_ storeReturn: @escaping (Any) -> Void, onError storeError: @escaping (Error) -> Void) {
    self.storeReturn = storeReturn
    self.storeError = storeError
  }

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print(" << onReturn: \(value)")
    storeReturn(value)
  }

  public func onReturnVoid() async throws {
    print(" << onReturnVoid: ()")
    storeReturn(())
  }

  public func onThrow<Err: Error>(error: Err) async throws {
    print(" << onThrow: \(error)")
    storeError(error)
  }
}

// ==== CustomSerializationProtocol Transport ----------------------------------

// Suppress Copyable like "new Codable" proposal's JSONCodable does
public protocol CustomSerializationProtocol: ~Copyable {
  func toBytes() throws -> [UInt8]
  static func fromBytes(_ bytes: [UInt8]) throws -> Self
}

extension ActorAddress {
  func toBytes() throws -> [UInt8] {
    var bytes: [UInt8] = []
    bytes.reserveCapacity(address.count)
    address.utf8CString.withUnsafeBytes { bs in
      for b in bs {
        bytes.append(b)
      }
    }
    return bytes
  }
  func fromBytes(_ bytes: [UInt8]) throws -> ActorAddress {
    let address = String(decoding: bytes, as: UTF8.self)
    return Self.init(parse: address)
  }
}

@available(SwiftStdlib 5.7, *)
public final class FakeCustomSerializationRoundtripActorSystem: DistributedActorSystem, @unchecked Sendable {
  public typealias ActorID = ActorAddress
  public typealias InvocationEncoder = FakeCustomSerializationInvocationEncoder
  public typealias InvocationDecoder = FakeCustomSerializationInvocationDecoder
  public typealias SerializationRequirement = CustomSerializationProtocol
  public typealias ResultHandler = FakeCustomSerializationRoundtripResultHandler

  var activeActors: [ActorID: any DistributedActor] = [:]
  var forcedNextRemoteCallReply: Any? = nil

  public init() {}

  public func shutdown() {
    self.activeActors = [:]
  }

  public func resolve<Act>(id: ActorID, as actorType: Act.Type)
    throws -> Act? where Act: DistributedActor {
    print("| resolve \(id) as remote // this system always resolves as remote")
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


  public func forceNextRemoteCallReply(_ reply: Any) {
    self.forcedNextRemoteCallReply = reply
  }

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
    print("  >> remoteCall: on:\(actor), target:\(target), invocation:\(invocation), throwing:\(String(reflecting: errorType)), returning:\(String(reflecting: returnType))")
    print(" > execute distributed target: \(target), identifier: \(target.identifier)")
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor: \(target.identifier)")
    }

    if let forcedNextRemoteCallReply {
      defer { self.forcedNextRemoteCallReply = nil }
      return forcedNextRemoteCallReply as! Res
    }

    func doIt<A: DistributedActor>(active: A) async throws -> Res {
      guard (actor.id) == active.id as! ActorID else {
        fatalError("Attempted to call mock 'roundtrip' on unknown actor: \(actor.id), known: \(active.id)")
      }

      let reply = FakeRoundtripReply()
      let resultHandler = FakeCustomSerializationRoundtripResultHandler { value in
        reply.result = value
        reply.error = nil
      } onError: { error in
        reply.result = nil
        reply.error = error
      }

      // The envelope which is sent to the recipient
      let envelope = FakeTargetEnvelope(target)
      var decoder = invocation.makeDecoder()

      let recipientTarget = fakeRecipientTarget(envelope)
      try await executeDistributedTarget(
        on: active,
        target: recipientTarget,
        invocationDecoder: &decoder,
        handler: resultHandler
      )

      switch (reply.result, reply.error) {
      case (.some(let value), nil):
        print("  << remoteCall return: \(value)")
        return reply.result! as! Res
      case (nil, .some(let error)):
        print("  << remoteCall throw: \(error)")
        throw error
      default:
        fatalError("No reply!")
      }
    }
    return try await _openExistential(targetActor, do: doIt)
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
    print("  >> remoteCallVoid: on:\(actor), target:\(target), invocation:\(invocation), throwing:\(String(reflecting: errorType))")
    guard let targetActor = activeActors[actor.id] else {
      fatalError("Attempted to call mock 'roundtrip' on: \(actor.id) without active actor")
    }

    func doIt<A: DistributedActor>(active: A) async throws {
      guard (actor.id) == active.id as! ActorID else {
        fatalError("Attempted to call mock 'roundtrip' on unknown actor: \(actor.id), known: \(active.id)")
      }

      let reply = FakeRoundtripReply()
      let resultHandler = FakeCustomSerializationRoundtripResultHandler { value in
        reply.result = value
        reply.error = nil
      } onError: { error in
        reply.result = nil
        reply.error = error
      }

      // The envelope which is sent to the recipient
      let envelope = FakeTargetEnvelope(target)
      var decoder = invocation.makeDecoder()

      print(" > execute distributed target: \(target)")
      let recipientTarget = fakeRecipientTarget(envelope)
      try await executeDistributedTarget(
        on: active,
        target: recipientTarget,
        invocationDecoder: &decoder,
        handler: resultHandler
      )

      switch (reply.result, reply.error) {
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

@available(SwiftStdlib 5.7, *)
public struct FakeCustomSerializationInvocationEncoder : DistributedTargetInvocationEncoder {
  public typealias SerializationRequirement = CustomSerializationProtocol

  var genericSubs: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
    print(" > encode generic sub: \(String(reflecting: type))")
    genericSubs.append(type)
  }

  public mutating func recordArgument<Value: SerializationRequirement>(
    _ argument: RemoteCallArgument<Value>) throws {
    print(" > encode argument name:\(argument.label ?? "_"), value: \(argument.value)")
    arguments.append(argument.value)
  }

  public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
    print(" > encode error type: \(String(reflecting: type))")
    self.errorType = type
  }

  public mutating func recordReturnType<R: SerializationRequirement>(_ type: R.Type) throws {
    print(" > encode return type: \(String(reflecting: type))")
    self.returnType = type
  }

  public mutating func doneRecording() throws {
    print(" > done recording")
  }

  public mutating func makeDecoder() -> FakeCustomSerializationInvocationDecoder {
    defer {
      // reset the decoder; we don't want to keep these values retained by accident here
      genericSubs = []
      arguments = []
      returnType = nil
      errorType = nil
    }
    return .init(
      args: arguments,
      substitutions: genericSubs,
      returnType: returnType,
      errorType: errorType
    )
  }
}

// === decoding --------------------------------------------------------------

// !!! WARNING !!!
// This is a 'final class' on purpose, to see that we retain the ad-hoc witness
// for 'decodeNextArgument'; Do not change it to just a class!
@available(SwiftStdlib 5.7, *)
public final class FakeCustomSerializationInvocationDecoder: DistributedTargetInvocationDecoder {
  public typealias SerializationRequirement = CustomSerializationProtocol

  var genericSubs: [Any.Type] = []
  var arguments: [Any] = []
  var returnType: Any.Type? = nil
  var errorType: Any.Type? = nil

  var argumentIndex: Int = 0

  fileprivate init(
    args: [Any],
    substitutions: [Any.Type] = [],
    returnType: Any.Type? = nil,
    errorType: Any.Type? = nil
  ) {
    self.arguments = args
    self.genericSubs = substitutions
    self.returnType = returnType
    self.errorType = errorType
  }

  public func decodeGenericSubstitutions() throws -> [Any.Type] {
    print("  > decode generic subs: \(genericSubs)")
    return genericSubs
  }

  public func decodeNextArgument<Argument: SerializationRequirement>() throws -> Argument {
    guard argumentIndex < arguments.count else {
      fatalError("Attempted to decode more arguments than stored! Index: \(argumentIndex), args: \(arguments)")
    }

    let anyArgument = arguments[argumentIndex]
    guard let argument = anyArgument as? Argument else {
      fatalError("Cannot cast argument\(anyArgument) to expected \(Argument.self)")
    }

    print("  > decode argument: \(argument)")
    argumentIndex += 1
    return argument
  }

  public func decodeErrorType() throws -> Any.Type? {
    print("  > decode return type: \(errorType.map { String(reflecting: $0) }  ?? "nil")")
    return self.errorType
  }

  public func decodeReturnType() throws -> Any.Type? {
    print("  > decode return type: \(returnType.map { String(reflecting: $0) }  ?? "nil")")
    return self.returnType
  }
}

@available(SwiftStdlib 5.7, *)
public struct FakeCustomSerializationRoundtripResultHandler: DistributedTargetInvocationResultHandler {
  public typealias SerializationRequirement = CustomSerializationProtocol

  let storeReturn: (any Any) -> Void
  let storeError: (any Error) -> Void
  init(_ storeReturn: @escaping (Any) -> Void, onError storeError: @escaping (Error) -> Void) {
    self.storeReturn = storeReturn
    self.storeError = storeError
  }

  public func onReturn<Success: SerializationRequirement>(value: Success) async throws {
    print(" << onReturn: \(value)")
    storeReturn(value)
  }

  public func onReturnVoid() async throws {
    print(" << onReturnVoid: ()")
    storeReturn(())
  }

  public func onThrow<Err: Error>(error: Err) async throws {
    print(" << onThrow: \(error)")
    storeError(error)
  }
}

// ==== Helpers ----------------------------------------------------------------

@available(SwiftStdlib 5.7, *)
@_silgen_name("swift_distributed_actor_is_remote")
func __isRemoteActor(_ actor: AnyObject) -> Bool

@available(SwiftStdlib 5.7, *)
func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}
