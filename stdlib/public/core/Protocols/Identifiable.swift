//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A class of types whose instances hold the value of an entity with stable
/// identity.
///
/// Use the `Identifiable` protocol to provide a stable notion of identity to a 
/// class or value type. For example, you could define a `User` type with an `id`
/// property that is stable across your app and your app's database storage. 
/// You could use the `id` property to identify a particular user even if other 
/// data fields change, such as the user's name.
///
/// `Identifiable` leaves the duration and scope of the identity unspecified.
/// Identities can have any of the following characteristics:
///
/// - Guaranteed always unique, like UUIDs.
/// - Persistently unique per environment, like database record keys.
/// - Unique for the lifetime of a process, like global incrementing integers.
/// - Unique for the lifetime of an object, like object identifiers.
/// - Unique within the current collection, like collection indices.
///
/// It's up to both the conformer and the receiver of the protocol to document
/// the nature of the identity.
///
/// Conforming to the Identifiable Protocol
/// =======================================
///
/// `Identifiable` provides a default implementation for class types (using
/// `ObjectIdentifier`), which is only guaranteed to remain unique for the
/// lifetime of an object. If an object has a stronger notion of identity, it
/// may be appropriate to provide a custom implementation.
@available(SwiftStdlib 5.1, *)
public protocol Identifiable<ID> {

  /// A type representing the stable identity of the entity associated with
  /// an instance.
  associatedtype ID: Hashable

  /// The stable identity of the entity associated with this instance.
  var id: ID { get }
}

@available(SwiftStdlib 5.1, *)
extension Identifiable where Self: AnyObject {
  public var id: ObjectIdentifier {
    return ObjectIdentifier(self)
  }
}
