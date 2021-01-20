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
@_implementationOnly import _SwiftConcurrencyShims

/// Common protocol to which all distributed actor classes conform.
///
/// The \c DistributedActor protocol provides the core functionality of any
/// distributed actor, which involves transforming actor
/// which involves enqueuing new partial tasks to be executed at some
/// point. Actor classes implicitly conform to this protocol as part of their
/// primary class definition.
public protocol DistributedActor: Actor, Codable {

  /// Creates new (local) distributed actor instance, bound to the passed transport.
  ///
  /// Upon initialization, the `actorAddress` field is populated by the transport,
  /// with an address assigned to this actor.
  ///
  /// - Parameter transport:
  init(transport: ActorTransport)

  /// Resolves the passed in `address` against the `transport`,
  /// returning either a local or remote actor reference.
  ///
  /// The transport will be asked to `resolve` the address and return either
  /// a local instance or determine that a proxy instance should be created
  /// for this address. A proxy actor will forward all invocations through
  /// the transport, allowing it to take over the remote messaging with the
  /// remote actor instance.
  ///
  /// - Parameter address:
  /// - Parameter transport:
  init(resolve address: ActorAddress, using transport: ActorTransport) // TODO: async

//  /// Decode an actor (remote or local) reference using the transport stored
//  /// within the passed decoder. The transport's `resolve` function is called to
//  /// perform all the heavy lifting of this function.
//  ///
//  /// Requires `ActorTransport` to be present as userInfo under the ... key.
//  override init(from decoder: Decoder) throws
//
//  @actorIndependent
//  override func encode(to encoder: Encoder) throws
//  // /Users/ktoso/code/swift-project-distributed/swift/stdlib/public/Concurrency/DistributedActor.swift:39:17: error: actor-independent instance method 'encode(to:)' has different actor isolation from non-actor-isolated overridden declaration
//  //  override func encode(to encoder: Encoder) throws
//  //                ^
//  // Swift.Encodable:2:10: note: overridden declaration is here
//  //    func encode(to encoder: Encoder) throws
//  //         ^

  @actorIndependent
  var actorTransport: ActorTransport { get }

  /// Logical address which this distributed actor represents.
  ///
  /// An address is always uniquely pointing at a specific actor instance
  @actorIndependent
  var actorAddress: ActorAddress { get }
}

// ==== Codable conformance ----------------------------------------------------

extension CodingUserInfoKey {
  static let actorTransport = CodingUserInfoKey(rawValue: "$dist_act_trans")!
}

extension DistributedActor {
  public init(from decoder: Decoder) throws {
    fatalError("DistributedActor.init(from decoder) NOT IMPLEMENTED")
//    guard let transport = decoder.userInfo["transport"] else {
//      throw DistributedActorDecodingError()
//    }
//    self =
  }

  @actorIndependent
  public func encode(to encoder: Encoder) throws {
//    var container = encoder.singleContainer()
//    container.encode(self.actorAddress)
    fatalError("DistributedActor.init(from decoder) NOT IMPLEMENTED")
  }
}

// TODO: implement in C, by inspecting the status flag of the instance
public func __isRemoteActor<Act>(_ actor: Act) -> Bool
  where Act: DistributedActor {
  return false // TODO: implement
}

public func __isLocalActor<Act>(_ actor: Act) -> Bool
  where Act: DistributedActor {
  return !__isRemoteActor(actor)
}

public protocol ActorTransport {
  /// Resolve a local or remote actor address to a real actor instance, or throw if unable to.
  /// The returned value is either a local actor or proxy to a remote actor.
  func resolve<Act>(address: ActorAddress, as actorType: Act.Type)
    throws -> Act where Act: DistributedActor

  /// Create an `ActorAddress` for the passed actor type.
  ///
  /// This function is invoked by an distributed actor during its initialization,
  /// and the returned address value is stored along with it for the time of its
  /// lifetime.
  ///
  /// The address MUST uniquely identify the actor, and allow resolving it.
  /// E.g. if an actor is created under address `addr1` then immediately invoking
  /// `transport.resolve(address: addr1, as: Greeter.self)` MUST return a reference
  /// to the same actor.
  func assignAddress<Act>(forType: Act.Type, onActorCreated: (Act) -> ()) -> ActorAddress
    where Act: DistributedActor

  // TODO: remove?
  func send<Message>(
    _ message: Message,
    to recipient: ActorAddress
  ) async throws where Message: Codable

  // TODO: remove?
  func request<Request, Reply>(
    replyType: Reply.Type,
    _ request: Request,
    from recipient: ActorAddress
  ) async throws where Request: Codable, Reply: Codable
}

public struct ActorAddress: Equatable, Codable {
  /// Uniquely specifies the actor transport and the protocol used by it.
  ///
  /// E.g. "xpc", "specific-clustering-protocol" etc.
  public var `protocol`: String

  public var host: String?
  public var port: Int?
  public var nodeID: UInt64?
  public var path: String?

  /// Unique Identifier of this actor.
  public var uid: UInt64 // TODO: should we remove this

  public init(parse: String) {
    self.protocol = "xxx"
    self.host = "xxx"
    self.port = 7337
    self.nodeID = 11
    self.path = "/example"
    self.uid = 123123
  }
}

/// Error protocol to which errors thrown by any `ActorTransport` should conform.
public protocol ActorTransportError: Error {}

public struct DistributedActorDecodingError: ActorTransportError {
  let message: String

  static func missingTransportUserInfo<Act>(_ actorType: Act.Type) -> DistributedActorDecodingError
    where Act: DistributedActor {
    .init(message: "Missing ActorTransport userInfo while decoding")
  }
}
