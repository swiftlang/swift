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

@available(SwiftStdlib 5.9, *)
public protocol Observable {
  nonisolated func transactions<Delivery: Actor>(
    for properties: TrackedProperties<Self>,
    isolation: Delivery
  ) -> ObservedTransactions<Self, Delivery>
  
  nonisolated func changes<Member: Sendable>(
    for keyPath: KeyPath<Self, Member>
  ) -> ObservedChanges<Self, Member>
  
  nonisolated static func dependencies(
    of keyPath: PartialKeyPath<Self>
  ) -> TrackedProperties<Self>
}


@available(SwiftStdlib 5.9, *)
extension Observable {
  public nonisolated func transactions<Member, Delivery: Actor>(
    for keyPath: KeyPath<Self, Member>,
    isolation: Delivery
  ) -> ObservedTransactions<Self, Delivery> {
    transactions(for: [keyPath], isolation: isolation)
  }
  
  public nonisolated func transactions(
    for properties: TrackedProperties<Self>
  ) -> ObservedTransactions<Self, MainActor.ActorType> {
    transactions(for: properties, isolation: MainActor.shared)
  }
  
  public nonisolated func transactions<Member>(
    for keyPath: KeyPath<Self, Member>
  ) -> ObservedTransactions<Self, MainActor.ActorType> {
    transactions(for: [keyPath], isolation: MainActor.shared)
  }
  
  public nonisolated static func dependencies(
    of keyPath: PartialKeyPath<Self>
  ) -> TrackedProperties<Self> {
    return [keyPath]
  }
}
