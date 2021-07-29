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

@available(SwiftStdlib 5.5, *)
public protocol ActorTransport: Sendable {

  // ==== ---------------------------------------------------------------------
  // - MARK: Resolving actors by identity

  /// Upon decoding of a `AnyActorIdentity` this function will be called
  /// on the transport that is set un the Decoder's `userInfo` at decoding time.
  /// This user info is to be set by the transport, as it receives messages and
  /// attempts decoding them. This way, messages received on a specific transport
  /// (which may be representing a connection, or general transport mechanism)
  /// is able to utilize local knowledge and type information about what kind of
  /// identity to attempt to decode.
  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity

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
  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type) throws -> ActorResolved<Act>
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
  // FIXME: make it Act.ID needs changes in AST gen
  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor
//  func assignIdentity<Act>(_ actorType: Act.Type) -> Act.ID
//      where Act: DistributedActor

  func actorReady<Act>(_ actor: Act) where Act: DistributedActor

  /// Called during actor deinit/destroy.
  func resignIdentity(_ id: AnyActorIdentity)

}

@available(SwiftStdlib 5.5, *)
public enum ActorResolved<Act: DistributedActor> {
  case resolved(Act)
  case makeProxy
}
