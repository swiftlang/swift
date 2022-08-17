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
/// The `DistributedActor` protocol generalizes over all distributed actor types.
/// Distributed actor types implicitly conform to this protocol.
///
/// It is not possible to conform to this protocol manually by any other type
/// than a `distributed actor`.
///
/// It is possible to require a type conform to the
/// ``DistributedActor`` protocol by refining it with another protocol,
/// or by using a generic constraint.
///
///
/// ## The ActorSystem associated type
/// Every distributed actor must declare what type of distributed actor system
/// it is part of by implementing the ``ActorSystem`` associated type requirement.
///
/// This causes a number of other properties of the actor to be inferred:
///   - the ``SerializationRequirement`` that will be used at compile time to
///     verify `distributed` target declarations are well formed,
///   - if the distributed actor is `Codable`, based on the ``ID`` being Codable or not,
///   - the type of the `ActorSystem` accepted in the synthesized default initializer.
///
/// A distributed actor must declare what type of actor system it is ready to
/// work with by fulfilling the ``ActorSystem`` type member requirement:
///
/// ```swift
/// distributed actor Greeter {
///   typealias ActorSystem = GreetingSystem // which conforms to DistributedActorSystem
///
///   func greet() -> String { "Hello!" }
/// }
/// ```
///
/// ### The DefaultDistributedActorSystem type alias
/// Since it is fairly common to only be using one specific type of actor system
/// within a module or entire codebase, it is possible to declare the default type
/// or actor system all distributed actors will be using in a module by declaring
/// a `DefaultDistributedActorSystem` module wide typealias:
///
/// ```swift
/// import Distributed
/// import AmazingActorSystemLibrary
///
/// typealias DefaultDistributedActorSystem = AmazingActorSystem
///
/// distributed actor Greeter {} // ActorSystem == AmazingActorSystem
/// ```
///
/// This declaration makes all `distributed actor` declarations,
/// which do not explicitly specify an ``ActorSystem`` typealias, to assume the
/// `AmazingActorSystem` as their `ActorSystem`.
///
/// It is possible for a specific actor to override the system it is using,
/// by declaring an ``ActorSystem`` type alias as usual:
///
/// ```swift
/// typealias DefaultDistributedActorSystem = AmazingActorSystem
///
/// distributed actor Amazing {
///   // ActorSystem == AmazingActorSystem
/// }
///
/// distributed actor Superb {
///   typealias ActorSystem = SuperbActorSystem
/// }
/// ```
///
/// In general the `DefaultDistributedActorSystem` should not be declared public,
/// as picking the default should be left up to each specific module of a project.
///
/// ## Default initializer
/// While classes and actors receive a synthesized *argument-free default
/// initializer* (`init()`), distributed actors synthesize a default initializer
/// that accepts a distributed actor system the actor is part of: `init(actorSystem:)`.
///
/// The accepted actor system must be of the `Self.ActorSystem` type, which
/// must conform to the ``DistributedActorSystem`` protocol. This is required
/// because of how distributed actors are always managed by, a concrete
/// distributed actor system, and cannot exist on their own without one.
///
/// It is possible to explicitly declare an parameter-free initializer (`init()`),
/// however the `actorSystem` property still must be assigned a concrete actor
/// system instance the actor shall be part of.
///
/// In general it is recommended to always have an `actorSystem` parameter as
/// the last non-defaulted non-closure parameter in every distributed actors
/// initializer parameter list. This way it is simple to swap in a "test actor
// system" instance in unit tests, and avoid relying on global state which could
/// make testing more difficult.
///
/// ## Implicit properties
/// Every concrete `distributed actor` type receives two synthesized properties,
/// which implement the protocol requirements of this protocol: `id` and `actorSystem`.
///
/// ### Property: Actor System
/// The ``actorSystem`` property is an important part of every distributed actor's lifecycle management.
/// Initialization as well as de-initialization both require interactions with the actor system,
/// and it is the actor system that delivers
///
///
/// The ``actorSystem`` property must be assigned in every designated initializer
/// of a distributed actor explicitly. It is highly recommended to make it a
/// parameter of every distributed actor initializer, and simply forward the
/// value to the stored property, like this:
///
/// ```swift
/// init(name: String, actorSystem: Self.ActorSystem) {
///   self.name = name
///   self.actorSystem = actorSystem
/// }
/// ```
///
/// ### Property: Distributed Actor Identity
/// The ``id`` is assigned by the actor system during the distributed actor's
/// initialization, and cannot be set or mutated by the actor itself.
///
/// The ``id`` is the effective identity of the actor, and is used in equality checks.
///
/// ## Automatic Conformances
///
/// ### Hashable and Identifiable conformance
/// Every distributed actor conforms to the Hashable and Identifiable protocols.
/// Its identity is strictly driven by its `id`, and therefore hash and equality
/// implementations directly delegate to the `id` property.
///
/// Comparing a local distributed actor instance and a remote reference to it
/// (both using the same ``id``) always returns true, as they both conceptually
/// point at the same distributed actor.
///
/// It is not possible to implement those protocols relying on the actual actor's
/// state, because it may be remote and the state may not be available. In other
/// words, since these protocols must be implemented using `nonisolated` functions,
/// only `nonisolated` `id` and `actorSystem` properties are accessible for their
/// implementations.
///
/// ### Implicit `Codable` conformance
/// If created with an actor system whose `ActorID` is `Codable`, the
/// compiler will synthesize code for the concrete distributed actor to conform
/// to `Codable` as well.
///
/// This is necessary to support distributed calls where the `SerializationRequirement`
/// is `Codable` and thus users may want to pass actors as arguments to remote calls.
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
///
/// - SeeAlso: ``DistributedActorSystem``
/// - SeeAlso: ``Actor``
/// - SeeAlso: ``AnyActor``
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

  /// A distributed actor's hash and equality is implemented by directly delegating to it's ``id``.
  nonisolated public func hash(into hasher: inout Hasher) {
    self.id.hash(into: &hasher)
  }

  /// A distributed actor's hash and equality is implemented by directly delegating to it's ``id``.
  nonisolated public static func ==(lhs: Self, rhs: Self) -> Bool {
    lhs.id == rhs.id
  }
}

// ==== Codable conformance ----------------------------------------------------

extension CodingUserInfoKey {

  /// Key which is required to be set on a `Decoder`'s `userInfo` while attempting
  /// to `init(from:)` a `DistributedActor`. The stored value under this key must
  /// conform to ``DistributedActorSystem``.
  ///
  /// Missing to set this key will result in that initializer throwing, because
  /// an actor system is required
  @available(SwiftStdlib 5.7, *)
  public static let actorSystemKey = CodingUserInfoKey(rawValue: "$distributed_actor_system")!
}

@available(SwiftStdlib 5.7, *)
extension DistributedActor /*: implicitly Decodable */ where Self.ID: Decodable {

  /// Initializes an instance of this distributed actor by decoding its ``id``,
  /// and passing it to the ``DistributedActorSystem`` obtained from `decoder.userInfo[`actorSystemKey]`.
  ///
  /// ## Requires: The decoder must have the ``CodingUserInfoKey/actorSystemKey`` set to
  /// the ``ActorSystem`` that this actor expects, as it will be used to call ``DistributedActor/resolve(id:using:)``
  /// on, in order to obtain the instance this initializer should return.
  ///
  /// - Parameter decoder: used to decode the ``ID`` of this distributed actor.
  /// - Throws: If the actor system value in `decoder.userInfo` is missing, or mis-typed;
  ///           the `ID` fails to decode from the passed `decoder`;
  //            or if the ``DistributedActor/resolve(id:using:)`` method invoked by this initializer throws.
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

  /// Encodes the ``actor.id`` as a single value into the passed `encoder`.
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
  public nonisolated func whenLocal<T: Sendable>(
    _ body: @Sendable (isolated Self) async throws -> T
  ) async rethrows -> T? {
    if __isLocalActor(self) {
       return try await body(self)
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
