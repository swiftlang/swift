//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
import _Concurrency

// ==== Distributed Actor -----------------------------------------------------

/// Common protocol to which all distributed actors conform implicitly.
///
/// It is not possible to conform to this protocol manually explicitly.
/// Only a 'distributed actor' declaration or protocol with 'DistributedActor'
/// requirement may conform to this protocol.
///
/// The 'DistributedActor' protocol provides the core functionality of any
/// distributed actor.
///
/// ## Implicit `Codable` conformance
/// If created with an actor system whose `ActorID` is `Codable`, the
/// compiler will synthesize code for the concrete distributed actor to conform
/// to `Codable` as well. This is necessary to support distributed calls where
/// the `SerializationRequirement` is `Codable` and thus users may want to pass
/// actors as arguments to remote calls.
///
/// The synthesized implementations use a single `SingleValueContainer` to
/// encode/decode the `self.id` property of the actor. The `Decoder` required
/// `init(from:)` is implemented by retrieving an actor system from the
/// decoders' `userInfo`, effectively like this:
/// `decoder.userInfo[.actorSystemKey] as? ActorSystem`. The obtained actor
/// system is then used to `resolve(id:using:)` the decoded ID.
///
/// Use the `CodingUserInfoKey.actorSystemKey` to provide the necessary
/// actor system for the decoding initializer when decoding a distributed actor.
@available(SwiftStdlib 5.7, *)
public protocol DistributedActor: AnyActor, Identifiable, Hashable
  where ID == ActorSystem.ActorID,
        SerializationRequirement == ActorSystem.SerializationRequirement {
  
  /// The type of transport used to communicate with actors of this type.
  associatedtype ActorSystem: DistributedActorSystem

  /// The serialization requirement to apply to all distributed declarations inside the actor.
  associatedtype SerializationRequirement

  /// Logical identity of this distributed actor.
  ///
  /// Many distributed actor references may be pointing at, logically, the same actor.
  /// For example, calling `resolve(id:using:)` multiple times, is not guaranteed
  /// to return the same exact resolved actor instance, however all the references would
  /// represent logically references to the same distributed actor, e.g. on a different node.
  ///
  /// Conformance to this requirement is synthesized automatically for any
  /// `distributed actor` declaration.
  nonisolated override var id: ID { get }

  /// The `ActorSystem` that is managing this distributed actor.
  ///
  /// It is immutable and equal to the system passed in the local/resolve
  /// initializer.
  ///
  /// Conformance to this requirement is synthesized automatically for any
  /// `distributed actor` declaration.
  nonisolated var actorSystem: ActorSystem { get }

  /// Resolves the passed in `id` against the `system`, returning
  /// either a local or remote actor reference.
  ///
  /// The system will be asked to `resolve` the identity and return either
  /// a local instance or request a proxy to be created for this identity.
  ///
  /// A remote distributed actor reference will forward all invocations through
  /// the system, allowing it to take over the remote messaging with the
  /// remote actor instance.
  ///
  /// - Parameter id: identity uniquely identifying a, potentially remote, actor in the system
  /// - Parameter system: `system` which should be used to resolve the `identity`, and be associated with the returned actor
  static func resolve(id: ID, using system: ActorSystem) throws -> Self
}

// ==== Hashable conformance ---------------------------------------------------

@available(SwiftStdlib 5.7, *)
extension DistributedActor {
  nonisolated public func hash(into hasher: inout Hasher) {
    self.id.hash(into: &hasher)
  }

  nonisolated public static func ==(lhs: Self, rhs: Self) -> Bool {
    lhs.id == rhs.id
  }
}

// ==== Codable conformance ----------------------------------------------------

extension CodingUserInfoKey {
  @available(SwiftStdlib 5.7, *)
  public static let actorSystemKey = CodingUserInfoKey(rawValue: "$distributed_actor_system")!
}

@available(SwiftStdlib 5.7, *)
extension DistributedActor /*: implicitly Decodable */ where Self.ID: Decodable {
  nonisolated public init(from decoder: Decoder) throws {
    guard let system = decoder.userInfo[.actorSystemKey] as? ActorSystem else {
      throw DistributedActorCodingError(message:
      "Missing system (for key .actorSystemKey) " +
          "in Decoder.userInfo, while decoding \(Self.self).")
    }

    let id: ID = try Self.ID(from: decoder)
    self = try Self.resolve(id: id, using: system)
  }
}

@available(SwiftStdlib 5.7, *)
extension DistributedActor /*: implicitly Encodable */ where Self.ID: Encodable {
  nonisolated public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(self.id)
  }
}

// ==== Local actor special handling -------------------------------------------

@available(SwiftStdlib 5.7, *)
extension DistributedActor {

  /// Executes the passed 'body' only when the distributed actor is local instance.
  ///
  /// The `Self` passed to the body closure is isolated, meaning that the
  /// closure can be used to call non-distributed functions, or even access actor
  /// state.
  ///
  /// When the actor is remote, the closure won't be executed and this function will return nil.
//  public nonisolated func whenLocal<T: Sendable>(
//    _ body: @Sendable (isolated Self) async throws -> T
//  ) async rethrows -> T? {
//    if __isLocalActor(self) {
//       return try await body(self)
//    } else {
//      return nil
//    }
//  }

  public nonisolated func whenLocal<T: Sendable>(
    _ body: @Sendable (_local Self) async throws -> T
  ) async rethrows -> T? {
    if __isLocalActor(self) {
       return try await body(self)
    } else {
      return nil
    }
  }

  public nonisolated func whenLocal<T: Sendable>(
    _ body: @Sendable (_local Self) throws -> T
  ) rethrows -> T? {
    if __isLocalActor(self) {
       return try body(self)
    } else {
      return nil
    }
  }
}

/******************************************************************************/
/************************* Runtime Functions **********************************/
/******************************************************************************/

// ==== isRemote / isLocal -----------------------------------------------------

@_silgen_name("swift_distributed_actor_is_remote")
public func __isRemoteActor(_ actor: AnyObject) -> Bool

public func __isLocalActor(_ actor: AnyObject) -> Bool {
  return !__isRemoteActor(actor)
}

// ==== Proxy Actor lifecycle --------------------------------------------------

@_silgen_name("swift_distributedActor_remote_initialize")
func _distributedActorRemoteInitialize(_ actorType: Builtin.RawPointer) -> Any
