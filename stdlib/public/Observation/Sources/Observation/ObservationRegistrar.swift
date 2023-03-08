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
public struct ObservationRegistrar<Subject: Observable>: Sendable {
  fileprivate let context = Context()
  private let lifetime: Lifetime
  
  public init() {
    lifetime = Lifetime(state: context.state)
  }

  internal struct Context: Identifiable {
    fileprivate let state = _ManagedCriticalState(managing: State())
    
    internal var id: ObjectIdentifier { state.id }

    fileprivate init() { }
  }
  
  private final class Lifetime: @unchecked Sendable {
    private let state: _ManagedCriticalState<State>
    
    fileprivate init(state: _ManagedCriticalState<State>) {
      self.state = state
    }
    
    deinit {
      state.withCriticalRegion { $0.deinitialize() }
    }
  }
  
  fileprivate enum Phase {
    case willSet
    case didSet
    case complete
  }
  
  fileprivate enum ResumeAction {
    case resumeAndRemove(UnsafeContinuation<Subject?, Never>)
    case informAndRemove(@Sendable () -> Void)
  }
  
  fileprivate struct Next {
    enum Kind {
      case transaction(TrackedProperties<Subject>)
      case pendingTransaction(TrackedProperties<Subject>, UnsafeContinuation<TrackedProperties<Subject>?, Never>)
      case change(TrackedProperties<Subject>, UnsafeContinuation<Subject?, Never>)
      case tracking(TrackedProperties<Subject>, @Sendable () -> Void)
      case cancelled
    }
    
    fileprivate var kind: Kind
    fileprivate var collected: TrackedProperties<Subject>?
  }
  
  fileprivate struct State: @unchecked Sendable {
    fileprivate var generation = 0
    fileprivate var nexts = [Int: Next]()
    fileprivate var lookups = [PartialKeyPath<Subject>: Set<Int>]()
    fileprivate var terminal = false

    fileprivate init() { }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar {
  public func access<Member>(
    _ subject: Subject, 
    keyPath: KeyPath<Subject, Member>
  ) {
    if let trackingPtr = _ThreadLocal.value?
      .assumingMemoryBound(to: ObservationTracking._AccessList?.self) {
      if trackingPtr.pointee == nil {
        trackingPtr.pointee = ObservationTracking._AccessList()
      }
      trackingPtr.pointee?.addAccess(keyPath: keyPath, context: context)
    }
  }
  
  public func willSet<Member>(
    _ subject: Subject, 
    keyPath: KeyPath<Subject, Member>
  ) {
    let observers = context.state.withCriticalRegion { state in
      state.willSet(subject, keyPath: keyPath)
    }
    for observer in observers {
      observer()
    }
  }
  
  public func didSet<Member>(
    _ subject: Subject, 
    keyPath: KeyPath<Subject, Member>
  ) {
    context.state.withCriticalRegion { state in
      state.didSet(subject, keyPath: keyPath)
    }
  }
  
  public func withMutation<Member, T>(
    of subject: Subject, 
    keyPath: KeyPath<Subject, Member>, 
    _ mutation: () throws -> T
  ) rethrows -> T {
    willSet(subject, keyPath: keyPath)
    defer { didSet(subject, keyPath: keyPath) }
    return try mutation()
  }
  
  public func transactions<Delivery: Actor>(
    for properties: TrackedProperties<Subject>, 
    isolation: Delivery
  ) -> ObservedTransactions<Subject, Delivery> {
    ObservedTransactions(context, properties: properties, isolation: isolation)
  }
  
  public func changes<Member: Sendable>(
    for keyPath: KeyPath<Subject, Member>
  ) -> ObservedChanges<Subject, Member> {
    ObservedChanges(context, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.Context {
  internal func schedule<Delivery: Actor>(
    _ generation: Int, 
    isolation: isolated Delivery
  ) async -> TrackedProperties<Subject>? {
    return await withUnsafeContinuation { continuation in
      state.withCriticalRegion { state in
        state.complete(generation, continuation: continuation)
      }
    }
  }
  
  internal func nextTransaction<Delivery: Actor>(
    for properties: TrackedProperties<Subject>, 
    isolation: Delivery
  ) async -> TrackedProperties<Subject>? {
    let generation = state.withCriticalRegion { state in
      let generation = state.nextGeneration()
      state.insert(transaction: properties, generation: generation)
      return generation
    }
    return await withTaskCancellationHandler {
      return await schedule(generation, isolation: isolation)
    } onCancel: {
      state.withCriticalRegion { $0.cancel(generation) }
    }
  }
  
  internal func nextChange<Member: Sendable>(
    to keyPath: KeyPath<Subject, Member>, 
    properties: TrackedProperties<Subject>
  ) async -> Member? {
    let generation = state.withCriticalRegion { $0.nextGeneration() }
    let subject: Subject? = await withTaskCancellationHandler {
      await withUnsafeContinuation { continuation in
        state.withCriticalRegion { state in
          state.insert(change: properties, 
                       continuation: continuation, generation: generation)
        }
      }
    } onCancel: {
      state.withCriticalRegion { $0.cancel(generation) }
    }
    return subject.map { $0[keyPath: keyPath] }
  }
  
  internal func nextTracking(
    for properties: TrackedProperties<Subject>, 
    _ observer: @Sendable @escaping () -> Void
  ) -> Int {
    return state.withCriticalRegion { state in
      let generation = state.nextGeneration()
      state.insert(properties: properties, 
                   tracking: observer, generation: generation)
      return generation
    }
  }
  
  internal func cancel(_ generation: Int) {
    state.withCriticalRegion { $0.cancel(generation) }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.Next {
  fileprivate mutating func resume(
    keyPath: PartialKeyPath<Subject>, 
    phase: ObservationRegistrar.Phase
  ) -> ObservationRegistrar.ResumeAction? {
    kind.resume(keyPath: keyPath, phase: phase, collected: &collected)
  }
  
  fileprivate func remove(
    from lookup: inout [PartialKeyPath<Subject>: Set<Int>], 
    generation: Int
  ) {
    kind.remove(from: &lookup, generation: generation)
  }
  
  fileprivate func deinitialize() {
    kind.deinitialize()
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.Next.Kind {
  fileprivate mutating func resume(
    keyPath: PartialKeyPath<Subject>,
    phase: ObservationRegistrar.Phase, 
    collected: inout TrackedProperties<Subject>?
  ) -> ObservationRegistrar.ResumeAction? {
    switch (self, phase) {
    case (.transaction(let observedTrackedProperties), .willSet):
      if observedTrackedProperties.contains(keyPath) {
        if var properties = collected {
          properties.insert(keyPath)
          collected = properties
        } else {
          var properties = TrackedProperties<Subject>()
          properties.insert(keyPath)
          collected = properties
        }
      }
      return nil
    case (.pendingTransaction(let observedTrackedProperties, let continuation), .willSet):
      if observedTrackedProperties.contains(keyPath) {
        if var properties = collected {
          properties.insert(keyPath)
          continuation.resume(returning: properties)
        } else {
          var properties = TrackedProperties<Subject>()
          properties.insert(keyPath)
          continuation.resume(returning: properties)
        }
        self = .transaction(observedTrackedProperties)
      }
      
      return nil
    case (.change(let observedTrackedProperties, let continuation), .didSet):
      if observedTrackedProperties.contains(keyPath) {
        if var properties = collected {
          properties.insert(keyPath)
          collected = properties
        } else {
          var properties = TrackedProperties<Subject>()
          properties.insert(keyPath)
          collected = properties
        }
        return .resumeAndRemove(continuation)
      }
      return nil
    case (.tracking(let observedTrackedProperties, let observer), .willSet):
      if observedTrackedProperties.contains(keyPath) {
        if var properties = collected {
          properties.insert(keyPath)
          collected = properties
        } else {
          var properties = TrackedProperties<Subject>()
          properties.insert(keyPath)
          collected = properties
        }
        return .informAndRemove(observer)
      } else {
        return nil
      }
    default:
      return nil
    }
  }
  
  fileprivate func invalidate(
    properties: TrackedProperties<Subject>, 
    from lookup: inout [PartialKeyPath<Subject>: Set<Int>], 
    generation: Int
  ) {
    for raw in properties.raw {
      if var members = lookup[raw] {
        members.remove(generation)
        if members.isEmpty {
          lookup.removeValue(forKey: raw)
        } else {
          lookup[raw] = members
        }
      }
    }
  }
  
  fileprivate func remove(
    from lookup: inout [PartialKeyPath<Subject>: Set<Int>], 
    generation: Int
  ) {
    switch self {
    case .transaction(let properties):
      invalidate(properties: properties, from: &lookup, generation: generation)
    case .pendingTransaction(let properties, _):
      invalidate(properties: properties, from: &lookup, generation: generation)
    case .change(let properties, _):
      invalidate(properties: properties, from: &lookup, generation: generation)
    case .tracking(let properties, _):
      invalidate(properties: properties, from: &lookup, generation: generation)
    default:
      break
    }
  }
  
  fileprivate func deinitialize() {
    switch self {
    case .pendingTransaction(_, let continuation):
      continuation.resume(returning: nil)
    case .change(_, let continuation):
      continuation.resume(returning: nil)
    default:
      break
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.State {
  fileprivate mutating func nextGeneration() -> Int {
    defer { generation &+= 1 }
    return generation
  }
  
  fileprivate mutating func cancel(_ generation: Int) {
    if let existing = nexts.removeValue(forKey: generation) {
      existing.remove(from: &lookups, generation: generation)
      existing.deinitialize()
    } else {
      nexts[generation] = ObservationRegistrar.Next(kind: .cancelled)
    }
  }
  
  fileprivate mutating func insert(
    transaction properties: TrackedProperties<Subject>, 
    generation: Int
  ) {
    if let existing = nexts.removeValue(forKey: generation) {
      switch existing.kind {
      case .cancelled:
        return
      default:
        existing.remove(from: &lookups, generation: generation)
      }
    }
    nexts[generation] = ObservationRegistrar.Next(kind: .transaction(properties))
    for raw in properties.raw {
      lookups[raw, default: []].insert(generation)
    }
  }
  
  fileprivate mutating func insert(
    change properties: TrackedProperties<Subject>, 
    continuation: UnsafeContinuation<Subject?, Never>, 
    generation: Int
  ) {
    guard !terminal else {
      continuation.resume(returning: nil)
      return
    }
    if let existing = nexts.removeValue(forKey: generation) {
      switch existing.kind {
      case .cancelled:
        continuation.resume(returning: nil)
        return
      default:
        existing.remove(from: &lookups, generation: generation)
      }
    }
    nexts[generation] = 
      ObservationRegistrar.Next(kind: .change(properties, continuation))
    for raw in properties.raw {
      lookups[raw, default: []].insert(generation)
    }
  }
  
  fileprivate mutating func insert(
    properties: TrackedProperties<Subject>, 
    tracking: @Sendable @escaping () -> Void, 
    generation: Int
  ) {
    nexts[generation] = 
      ObservationRegistrar.Next(kind: .tracking(properties, tracking))
    for raw in properties.raw {
      lookups[raw, default: []].insert(generation)
    }
  }
  
  fileprivate mutating func complete(
    _ generation: Int,
    continuation: UnsafeContinuation<TrackedProperties<Subject>?, Never>
  ){
    if let existing = nexts.removeValue(forKey: generation) {
      switch existing.kind {
      case .transaction(let properties):
        if let collected = existing.collected {
          continuation.resume(returning: collected)
        } else {
          nexts[generation] = ObservationRegistrar.Next(kind: .pendingTransaction(properties, continuation))
        }
      default:
        continuation.resume(returning: nil)
      }
    }
  }
  
  fileprivate mutating func willSet<Member>(
    _ subject: Subject, 
    keyPath: KeyPath<Subject, Member>
  ) -> [() -> Void] {
    let raw = keyPath
    var observers = [() -> Void]()
    for generation in lookups[raw] ?? [] {
      if let resume = nexts[generation]?.resume(keyPath: raw, phase: .willSet) {
        switch resume {
        case .resumeAndRemove(let continuation):
          continuation.resume(returning: subject)
        case .informAndRemove(let observer):
          observers.append(observer)
        }
        if let existing = nexts.removeValue(forKey: generation) {
          existing.remove(from: &lookups, generation: generation)
        }
      }
    }
    return observers
  }
  
  fileprivate mutating func didSet<Member>(
    _ subject: Subject, 
    keyPath: KeyPath<Subject, Member>
  ) {
    let raw = keyPath
    guard let generations = lookups[raw] else {
      return
    }
    for generation in generations {
      if let resume = nexts[generation]?.resume(keyPath: raw, phase: .didSet) {
        switch resume {
        case .resumeAndRemove(let continuation):
          continuation.resume(returning: subject)
        default:
          break
        }
        if let existing = nexts.removeValue(forKey: generation) {
          existing.remove(from: &lookups, generation: generation)
        }
      }
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.State: _Deinitializable {
  fileprivate mutating func deinitialize() {
    terminal = true
    for next in nexts.values {
      next.deinitialize()
    }
    nexts.removeAll()
    lookups.removeAll()
  }
}
