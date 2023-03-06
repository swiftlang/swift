//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import _Concurrency

/// A type that provides access to changes to its properties over time.
@available(SwiftStdlib 5.9, *)
public protocol Observable {
  /// Returns an asynchronous sequence of change transactions for the specified
  /// properties, isolated to the given actor.
  ///
  /// Implement this method only if you are manually providing `Observable`
  /// conformance instead of using the `@Observable` macro.
  ///
  /// - Parameters:
  ///   - properties: A set of key paths indicating which properties should be
  ///     included in the tracked transactions.
  ///   - isolation: The actor on which to isolate transaction notifications.
  nonisolated func transactions<Delivery: Actor>(
    for properties: TrackedProperties<Self>,
    isolation: Delivery
  ) -> ObservedTransactions<Self, Delivery>
  
  /// Returns an asynchronous sequence of changes for the specified key path.
  ///
  /// Implement this method only if you are manually providing `Observable`
  /// conformance instead of using the `@Observable` macro.
  ///
  /// - Parameters:
  ///   - keyPath: The path to the property that should tracked by the
  ///   - isolation: The actor on which to isolate transaction notifications.
  nonisolated func changes<Member: Sendable>(
    for keyPath: KeyPath<Self, Member>
  ) -> ObservedChanges<Self, Member>
  
  /// Returns a set of tracked properties that represent the given key path's
  /// dependencies.
  ///
  /// Implement this method if your observable type includes computed properties
  /// that should be observable. For example, the `aspectRatio` property in the
  /// `Dimensions` type declaration below is computed from the `width` and
  /// `height` stored properties. The `dependencies(of:)` implementation returns
  /// a `TrackedProperties` instance representing those two key paths when
  /// `keyPath` is `\.aspectRatio`, or just the key path itself otherwise.
  ///
  ///     @Observable
  ///     class Dimensions {
  ///         var width: Double = 0.0
  ///         var height: Double = 0.0
  ///
  ///         var aspectRatio: Double {
  ///             width / height
  ///         }
  ///
  ///         nonisolated static func dependencies(
  ///             of keyPath: PartialKeyPath<Self>
  ///         ) -> TrackedProperties<Self> {
  ///             switch keyPath {
  ///             case \.aspectRatio:
  ///                 return [\.width, \.height]
  ///             default:
  ///                 return [keyPath]
  ///             }
  ///         }
  ///     }
  ///
  /// The default implementation of `dependencies(of:)` treats each property as
  /// self-contained.
  ///
  /// - Parameter keyPath: The key path to a property of this type.
  /// - Returns: A `TrackedProperties` instance that represents the key paths
  ///   of all properties that affect `keyPath`.
  nonisolated static func dependencies(
    of keyPath: PartialKeyPath<Self>
  ) -> TrackedProperties<Self>
}

@available(SwiftStdlib 5.9, *)
extension Observable {
  /// Returns an asynchronous sequence of change transactions for the specified
  /// key path, isolated to the given actor.
  ///
  /// - Parameters:
  ///   - keyPath: A key path indicating which property should be included in
  ///     the tracked transactions.
  ///   - isolation: The actor on which to isolate transaction notifications.
  public nonisolated func transactions<Member, Delivery: Actor>(
    for keyPath: KeyPath<Self, Member>,
    isolation: Delivery
  ) -> ObservedTransactions<Self, Delivery> {
    transactions(for: [keyPath], isolation: isolation)
  }
  
  /// Returns an asynchronous sequence of change transactions for the specified
  /// properties, isolated to the main actor.
  ///
  /// - Parameters:
  ///   - properties: A set of key paths indicating which properties should be
  ///     included in the tracked transactions.
  public nonisolated func transactions(
    for properties: TrackedProperties<Self>
  ) -> ObservedTransactions<Self, MainActor.ActorType> {
    transactions(for: properties, isolation: MainActor.shared)
  }
  
  /// Returns an asynchronous sequence of change transactions for the specified
  /// key path, isolated to the main actor.
  ///
  /// - Parameters:
  ///   - keyPath: A key path indicating which property should be included in
  ///     the tracked transactions.
  public nonisolated func transactions<Member>(
    for keyPath: KeyPath<Self, Member>
  ) -> ObservedTransactions<Self, MainActor.ActorType> {
    transactions(for: [keyPath], isolation: MainActor.shared)
  }

  // Documentation inherited from protocol requirement.
  public nonisolated static func dependencies(
    of keyPath: PartialKeyPath<Self>
  ) -> TrackedProperties<Self> {
    return [keyPath]
  }
}
