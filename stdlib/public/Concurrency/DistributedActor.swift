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
  init(transport: ActorTransport)

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

public func __isRemoteActor<Act>(_ actor: Act) -> Bool
  where Act: DistributedActor {
  return false // TODO: implement
}

public protocol ActorTransport {
  /// Resolve a local or remote actor address to a real actor instance, or throw if unable to.
  /// The returned value is either a local actor or proxy to a remote actor.
  func resolve<A>(address: ActorAddress, as actorType: A.Type) where A: DistributedActor

  func send<Message>(
    _ message: Message,
    to recipient: ActorAddress
  ) async throws where Message: Codable

  func request<Request, Reply>(
    replyType: Reply.Type,
    _ request: Request,
    from recipient: ActorAddress
  ) async throws where Request: Codable, Reply: Codable
}

public protocol ActorAddress: Codable {
  /// Uniquely specifies the actor transport and the protocol used by it.
  ///
  /// E.g. "xpc", "specific-clustering-protocol" etc.
  static var `protocol`: String { get }
  // TODO: node etc

  var host: String? { get }
  var port: Int? { get }

  /// Unique Identifier of this actor.
  var uid: UInt64 { get }
}

struct DistributedActorDecodingError: Error {
}
