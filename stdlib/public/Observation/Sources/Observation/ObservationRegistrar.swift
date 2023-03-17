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

  fileprivate let context = Context()
  private let lifetime: Lifetime
  
  public init() {
    lifetime = Lifetime(state: context.state)
  }

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
    let action = context.state.withCriticalRegion { state in
      state.willSet(subject, keyPath: keyPath)
    }
    if let action {
      switch action {
      case .tracking(let observers):
        for observer in observers {
          observer()
        }
      }
    }
  }
  
  public func didSet<Member>(
      _ subject: Subject,
      keyPath: KeyPath<Subject, Member>
  ) {
    let actions = context.state.withCriticalRegion { state in
      state.didSet(subject, keyPath: keyPath)
    }
    for action in actions {
      switch action {
      case .resumeWithMember(let continuation):
        continuation.resume(returning: subject[keyPath: keyPath])
      }
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
  
  public func changes<Isolation: Actor>(
    for properties: TrackedProperties<Subject>,
    isolatedTo isolation: Isolation
  ) -> ObservedChanges<Subject, Isolation> {
    ObservedChanges(context, properties: properties, isolation: isolation)
  }
  
  public func values<Member: Sendable>(
    for keyPath: KeyPath<Subject, Member>
  ) -> ObservedValues<Subject, Member> {
    ObservedValues(context, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar {
  enum ObservationKind {
    case transactions
    case changes
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar {
  struct State: @unchecked Sendable {
    enum Observation {
      case idleChanges(IdleChangesState)
      case pendingChange(PendingChangeState)
      case activeChange(ActiveChangeState)
      case idleValues(IdleValuesState)
      case pendingValue(PendingValueState)
      case activeValue(ActiveValueState)
      case tracking(TrackingState)
      case cancelled
    }
    
    var id: Int = 0
    var observations = [Int: Observation]()
    var lookups = [PartialKeyPath<Subject>: Set<Int>]()
    
    mutating func insert(
      _ kind: ObservationRegistrar.ObservationKind, 
      properties: TrackedProperties<Subject>
    ) -> Int {
      let id = self.id
      self.id = id + 1
      switch kind {
      case .transactions:
        observations[id] = Observation(idleChanges: properties)
      case .changes:
        observations[id] = Observation(idleValues: properties)
      }
      insert(for: properties, id: id)
      return id
    }
    
    mutating func removeLookups(
      _ properties: TrackedProperties<Subject>,
      id: Int
    ) {
      for keyPath in properties.raw {
        if var observationIds = lookups.removeValue(forKey: keyPath) {
          observationIds.remove(id)
          if observationIds.count > 0 {
            lookups[keyPath] = observationIds
          }
        }
      }
    }
    
    mutating func remove(_ id: Int) {
      if let observation = observations.removeValue(forKey: id) {
        switch observation {
        case .idleChanges(let observation):
          removeLookups(observation.properties, id: id)
        case .pendingChange(let observation):
          removeLookups(observation.properties, id: id)
        case .activeChange(let observation):
          removeLookups(observation.properties, id: id)
          observation.continuation.resume(returning: nil)
        case .idleValues:
          break
        case .pendingValue:
          break
        case .activeValue(let observation):
          removeLookups(observation.properties, id: id)
          func terminate<Member>(_ valueType: Member.Type) {
            unsafeBitCast(
              observation.rawContinuation,
              to: UnsafeContinuation<Member?, Never>.self
            ).resume(returning: nil)
          }
          _openExistential(type(of: observation.keyPath).valueType, do: terminate)
        case .tracking(let observation):
          removeLookups(observation.properties, id: id)
        case .cancelled:
          break
        }
      }
    }
    
    enum WillSetAction {
      case tracking([@Sendable () -> Void])
    }
    
    mutating func willSet<Member>(
      _ subject: Subject,
      keyPath: KeyPath<Subject, Member>
    ) -> WillSetAction? {
      var result = [@Sendable () -> Void]()
      if let ids = lookups[keyPath] {
        for id in ids {
          switch observations[id] {
          // the only participant of willSet is the tracking
          case .tracking(let observation):
            result.append(observation.observer)
            remove(id)
          default:
            break
          }
        }
      }
      if result.count > 0 {
        return .tracking(result)
      } else {
        return nil
      }
    }
    
    enum DidSetAction<Member> {
      case resumeWithMember(UnsafeContinuation<Member?, Never>)
    }
    
    mutating func didSet<Member>(
      _ subject: Subject,
      keyPath: KeyPath<Subject, Member>
    ) -> [DidSetAction<Member>] {
      var actions = [DidSetAction<Member>]()
      if let ids = lookups[keyPath] {
        for id in ids {
          switch observations[id] {
          case .idleChanges(let observation):
            let change =
              ObservedChange(subject: subject, properties: [keyPath])
            observations[id] = observation.transitionToPending(change)
            break
          case .none:
            fatalError("Internal inconsistency")
          case .pendingChange(let observation):
            observations[id] = observation.inserting(subject, keyPath)
          case .activeChange(let observation):
            let change = ObservedChange(subject: subject, properties: [keyPath])
            observation.continuation.resume(returning: change)
            observations[id] = observation.transitionToIdle()
          case .idleValues(let observation):
            observations[id] = observation.transitionToPending(subject)
          case .pendingValue:
            // the placeholder already has the subject of 
            // observation so no need to update
            break
          case .activeValue(let observation):
            observations[id] = observation.transitionToIdle()
            actions.append(
              .resumeWithMember(
                unsafeBitCast(
                  observation.rawContinuation,
                  to: UnsafeContinuation<Member?, Never>.self
                )
              )
            )
          case .tracking:
            // Tracking does not interplay with didSet
            break
          case .cancelled:
            // cancelled does not interplay with didSet
            break
          }
        }
      }
      return actions
    }
    
    mutating func beginTransaction(
      for properties: TrackedProperties<Subject>,
      id: Int
    ) {
      switch observations[id] {
      case .idleChanges(let observation):
        observations[id] = observation.transitionToPending()
      case .pendingChange:
        // beginning more than once just no-ops past the first
        break
      case .activeChange: fallthrough
      case .idleValues: fallthrough
      case .pendingValue: fallthrough
      case .activeValue: fallthrough
      case .tracking: fallthrough
      case .none:
        fatalError("Internal inconsistency")
      case .cancelled:
        // cancellation can happen at any time
        break
      }
    }
    
    mutating func insert(
      for properties: TrackedProperties<Subject>,
      id: Int
    ) {
      for keyPath in properties.raw {
        lookups[keyPath, default: []].insert(id)
      }
    }
    
    mutating func insertNextChange(
      for properties: TrackedProperties<Subject>,
      continuation: UnsafeContinuation<ObservedChange<Subject>?, Never>,
      id: Int
    ) {
      switch observations[id] {
      case .idleChanges(let observation):
        observations[id] = observation.transitionToActive(continuation)
      case .pendingChange(let observation):
        if let change = observation.change {
          observations[id] = observation.transitionToIdle()
          continuation.resume(returning: change)
        } else {
          observations[id] = observation.transitionToActive(continuation)
        }
      case .idleValues: fallthrough
      case .pendingValue: fallthrough
      case .activeValue: fallthrough
      case .tracking: fallthrough
      case .none:
        fatalError("Internal inconsistency")
      case .activeChange:
        fatalError("attempting to await more than once on a non-sendable iterator")
      case .cancelled:
        remove(id)
        continuation.resume(returning: nil)
      }
    }
    
    enum InsertChangeAction {
      case resumeWithMember(Subject)
    }
    
    mutating func insertNextValue<Member: Sendable>(
        for keyPath: KeyPath<Subject, Member>,
        properties: TrackedProperties<Subject>,
        continuation: UnsafeContinuation<Member?, Never>,
        id: Int
    ) -> InsertChangeAction? {
      switch observations[id] {
      case .idleValues(let observation):
        observations[id] = 
          observation.transitionToActive(keyPath, properties, continuation)
      case .pendingValue(let observation):
        observations[id] = observation.transitionToIdle()
        return .resumeWithMember(observation.subject)
      case .activeValue:
        fatalError("attempting to await more than once on a non-sendable iterator")
      case .idleChanges: fallthrough
      case .pendingChange: fallthrough
      case .activeChange: fallthrough
      case .tracking: fallthrough
      case .none:
        fatalError("Internal inconsistency")
      case .cancelled:
        remove(id)
        continuation.resume(returning: nil)
      }
      return nil
    }
    
    mutating func insertNextTracking(
      for properties: TrackedProperties<Subject>,
      _ observer: @Sendable @escaping () -> Void
    ) -> Int {
      let id = self.id
      self.id = id + 1
      observations[id] = Observation(tracking: properties, observer)
      insert(for: properties, id: id)
      return id
    }
    
    mutating func cancel(_ id: Int) {
      switch observations[id] {
      // Any observation state before active must identify it as cancelled
      // just in case an active state comes along later
      case .idleChanges: fallthrough
      case .pendingChange: fallthrough
      case .idleValues: fallthrough
      case .pendingValue:
        observations[id] = .cancelled
      case .activeChange: fallthrough
      case .activeValue:
        remove(id)
      case .tracking: fallthrough
      case .cancelled: fallthrough
      case .none:
        break
      }
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.State: _Deinitializable {
  mutating func deinitialize() {
    for id in observations.keys {
      remove(id)
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.Context {
  func register(
    _ kind: ObservationRegistrar.ObservationKind, 
    properties: TrackedProperties<Subject>
  ) -> Int {
    state.withCriticalRegion { $0.insert(kind, properties: properties) }
  }
  
  func unregister(_ id: Int) {
    state.withCriticalRegion { $0.remove(id) }
  }
  
  private func scheduleNextChange<Isolation: Actor>(
    for properties: TrackedProperties<Subject>,
    isolation: isolated Isolation,
    id: Int
  ) async -> ObservedChange<Subject>? {
    return await withUnsafeContinuation { continuation in
      state.withCriticalRegion { state in
        state.insertNextChange(
          for: properties, 
          continuation: continuation, 
          id: id
        )
      }
    }
  }
  
  internal func nextChange<Isolation: Actor>(
      for properties: TrackedProperties<Subject>,
      isolation: Isolation,
      id: Int
  ) async -> ObservedChange<Subject>? {
    await withTaskCancellationHandler {
      return await scheduleNextChange(
        for: properties, 
        isolation: isolation, 
        id: id
      )
    } onCancel: {
      state.withCriticalRegion { $0.cancel(id) }
    }
  }
  
  internal func nextValue<Member: Sendable>(
      for keyPath: KeyPath<Subject, Member>,
      properties: TrackedProperties<Subject>,
      id: Int
  ) async -> Member? {
    await withTaskCancellationHandler {
      await withUnsafeContinuation { continuation in
        let action = state.withCriticalRegion { state in
          state.insertNextValue(
            for: keyPath, 
            properties: properties, 
            continuation: continuation, 
            id: id
          )
        }
        if let action {
          switch action {
          case .resumeWithMember(let subject):
            continuation.resume(returning: subject[keyPath: keyPath])
          }
        }
      }
    } onCancel: {
      state.withCriticalRegion { $0.cancel(id) }
    }
  }
  
  internal func nextTracking(
    for properties: TrackedProperties<Subject>,
    _ observer: @Sendable @escaping () -> Void
  ) -> Int {
    state.withCriticalRegion { state in
      state.insertNextTracking(for: properties, observer)
    }
  }
  
  internal func cancel(_ id: Int) {
    state.withCriticalRegion { $0.cancel(id) }
  }
}
