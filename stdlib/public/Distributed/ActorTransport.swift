//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
import _Concurrency

@available(SwiftStdlib 5.6, *)
public protocol ActorTransport: Sendable {
  /// The identity used by actors that communicate via this transport
  associatedtype Identity: ActorIdentity = AnyActorIdentity

  // ==== ---------------------------------------------------------------------
  // - MARK: Resolving actors by identity

  /// Upon decoding of an `Identity` this function will be called
  /// on the transport that is set un the Decoder's `userInfo` at decoding time.
  /// This user info is to be set by the transport, as it receives messages and
  /// attempts decoding them. This way, messages received on a specific transport
  /// (which may be representing a connection, or general transport mechanism)
  /// is able to utilize local knowledge and type information about what kind of
  /// identity to attempt to decode.
  func decodeIdentity(from decoder: Decoder) throws -> Identity

  /// Resolve a local or remote actor address to a real actor instance, or throw if unable to.
  /// The returned value is either a local actor or proxy to a remote actor.
  ///
  /// Resolving an actor is called when a specific distributed actors `init(from:)`
  /// decoding initializer is invoked. Once the actor's identity is deserialized
  /// using the `decodeIdentity(from:)` call, it is fed into this function, which
  /// is responsible for resolving the identity to a remote or local actor reference.
  ///
  /// If the resolve fails, meaning that it cannot locate a local actor managed for
  /// this identity, managed by this transport, nor can a remote actor reference
  /// be created for this identity on this transport, then this function must throw.
  ///
  /// If this function returns correctly, the returned actor reference is immediately
  /// usable. It may not necessarily imply the strict *existence* of a remote actor
  /// the identity was pointing towards, e.g. when a remote system allocates actors
  /// lazily as they are first time messaged to, however this should not be a concern
  /// of the sending side.
  ///
  /// Detecting liveness of such remote actors shall be offered / by transport libraries
  /// by other means, such as "watching an actor for termination" or similar.
  func resolve<Act>(_ identity: Identity, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor

  // ==== ---------------------------------------------------------------------
  // - MARK: Actor Lifecycle

  /// Create an `ActorIdentity` for the passed actor type.
  ///
  /// This function is invoked by an distributed actor during its initialization,
  /// and the returned address value is stored along with it for the time of its
  /// lifetime.
  ///
  /// The address MUST uniquely identify the actor, and allow resolving it.
  /// E.g. if an actor is created under address `addr1` then immediately invoking
  /// `transport.resolve(address: addr1, as: Greeter.self)` MUST return a reference
  /// to the same actor.
  func assignIdentity<Act>(_ actorType: Act.Type) -> Identity
      where Act: DistributedActor

  func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor

  /// Called during actor deinit/destroy.
  func resignIdentity(_ id: Identity)
}

@available(SwiftStdlib 5.6, *)
extension ActorTransport {
  /// Try to resolve based on a type-erased actor identity.
  func resolve<Act>(
    anyIdentity identity: AnyActorIdentity, as actorType: Act.Type
  ) throws -> Act? where Act: DistributedActor {
    return try resolve(identity.underlying as! Identity, as: actorType)
  }

  /// Try to resign based on a type-erased actor identity.
  func resignAnyIdentity(_ id: AnyActorIdentity) {
    resignIdentity(id.underlying as! Identity)
  }

  func decodeAnyIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    AnyActorIdentity(try decodeIdentity(from: decoder))
  }

  func assignAnyIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    return AnyActorIdentity(assignIdentity(actorType))
  }
}

@available(SwiftStdlib 5.6, *)
public struct AnyActorTransport: ActorTransport {
  public typealias Identity = AnyActorIdentity

  let transport: ActorTransport

  public init<Transport: ActorTransport>(_ transport: Transport) {
    self.transport = transport
  }

  public func decodeIdentity(from decoder: Decoder) throws -> Identity {
    return try transport.decodeAnyIdentity(from: decoder)
  }

  public func resolve<Act>(
    _ identity: Identity, as actorType: Act.Type
  ) throws -> Act? where Act: DistributedActor {
    return try transport.resolve(anyIdentity: identity, as: actorType)
  }

  public func assignIdentity<Act>(_ actorType: Act.Type) -> Identity
      where Act: DistributedActor {
    return transport.assignAnyIdentity(actorType)
  }

  public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor {
    transport.actorReady(actor)
  }

  /// Called during actor deinit/destroy.
  public func resignIdentity(_ id: Identity) {
    transport.resignAnyIdentity(id)
  }
}

///// Use the existential wrapper as the default actor transport.
//@available(SwiftStdlib 5.6, *)
//public typealias DefaultActorTransport = AnyActorTransport
