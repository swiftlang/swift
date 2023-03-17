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
  nonisolated func changes<Isolation: Actor>(
    for properties: TrackedProperties<Self>,
    isolatedTo isolation: Isolation
  ) -> ObservedChanges<Self, Isolation>
  
  nonisolated func values<Member: Sendable>(
    for keyPath: KeyPath<Self, Member>
  ) -> ObservedValues<Self, Member>
  
  nonisolated static func dependencies(
    of keyPath: PartialKeyPath<Self>
  ) -> TrackedProperties<Self>
}


@available(SwiftStdlib 5.9, *)
extension Observable {
  public nonisolated func changes<Member, Isolation: Actor>(
    for keyPath: KeyPath<Self, Member>,
    isolatedTo isolation: Isolation
  ) -> ObservedChanges<Self, Isolation> {
    changes(for: [keyPath], isolatedTo: isolation)
  }
  
  public nonisolated func changes(
    for properties: TrackedProperties<Self>
  ) -> ObservedChanges<Self, MainActor.ActorType> {
    changes(for: properties, isolatedTo: MainActor.shared)
  }
  
  public nonisolated func changes<Member>(
    for keyPath: KeyPath<Self, Member>
  ) -> ObservedChanges<Self, MainActor.ActorType> {
    changes(for: [keyPath], isolatedTo: MainActor.shared)
  }
  
  public nonisolated static func dependencies(
    of keyPath: PartialKeyPath<Self>
  ) -> TrackedProperties<Self> {
    return [keyPath]
  }
}
