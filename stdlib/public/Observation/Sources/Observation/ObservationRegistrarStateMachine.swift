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
extension ObservationRegistrar {
  struct IdleChangesState {
    let properties: TrackedProperties<Subject>
    
    fileprivate init(properties: TrackedProperties<Subject>) {
      self.properties = properties
    }
    
    func transitionToPending(
      _ change: ObservedChange<Subject>? = nil
    ) -> State.Observation {
      return .pendingChange(
        PendingChangeState(properties: properties, change: change))
    }
    
    func transitionToActive(
      _ continuation: UnsafeContinuation<ObservedChange<Subject>?, Never>
    ) -> State.Observation {
      return .activeChange(
        ActiveChangeState(properties: properties, continuation: continuation))
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
  
  struct PendingChangeState {
    let properties: TrackedProperties<Subject>
    let change: ObservedChange<Subject>?
    
    fileprivate init(
      properties: TrackedProperties<Subject>, 
      change: ObservedChange<Subject>?
    ) {
      self.properties = properties
      self.change = change
    }
    
    func inserting(
      _ subject: Subject, 
      _ keyPath: PartialKeyPath<Subject>
    ) -> State.Observation {
      if var change {
        change.insert(keyPath)
        return .pendingChange(
          PendingChangeState(properties: properties, change: change))
      } else {
        let change = ObservedChange(subject: subject, properties: [keyPath])
        return .pendingChange(
          PendingChangeState(properties: properties, change: change))
      }
    }
    
    func transitionToIdle() -> State.Observation {
      return .idleChanges(IdleChangesState(properties: properties))
    }
    
    func transitionToActive(
      _ continuation: UnsafeContinuation<ObservedChange<Subject>?, Never>
    ) -> State.Observation {
      return .activeChange(
        ActiveChangeState(properties: properties, continuation: continuation))
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
  
  struct ActiveChangeState {
    let properties: TrackedProperties<Subject>
    let continuation: UnsafeContinuation<ObservedChange<Subject>?, Never>
    
    fileprivate init(
      properties: TrackedProperties<Subject>, 
      continuation: UnsafeContinuation<ObservedChange<Subject>?, Never>
    ) {
      self.properties = properties
      self.continuation = continuation
    }
    
    func transitionToIdle() -> State.Observation {
      return .idleChanges(IdleChangesState(properties: properties))
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
  
  struct IdleValuesState {
    fileprivate init() { }
    
    func transitionToPending(_ subject: Subject) -> State.Observation {
      return .pendingValue(PendingValueState(subject: subject))
    }
    
    func transitionToActive<Member: Sendable>(
      _ keyPath: KeyPath<Subject, Member>, 
      _ properties: TrackedProperties<Subject>, 
      _ continuation: UnsafeContinuation<Member?, Never>
    ) -> State.Observation {
      return .activeValue(
        ActiveValueState(
          properties: properties, 
          keyPath: keyPath, 
          continuation: continuation
        )
      )
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
  
  struct PendingValueState {
    let subject: Subject
    
    fileprivate init(subject: Subject) {
      self.subject = subject
    }
    
    func transitionToIdle() -> State.Observation {
      return .idleValues(IdleValuesState())
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
  
  struct ActiveValueState {
    let properties: TrackedProperties<Subject>
    let keyPath: PartialKeyPath<Subject>
    let rawContinuation: UnsafeRawPointer
    
    fileprivate init<Member>(
      properties: TrackedProperties<Subject>, 
      keyPath: KeyPath<Subject, Member>, 
      continuation: UnsafeContinuation<Member?, Never>
    ) {
      self.properties = properties
      self.keyPath = keyPath
      self.rawContinuation = 
        unsafeBitCast(continuation, to: UnsafeRawPointer.self)
    }
    
    func transitionToIdle() -> State.Observation {
      return .idleValues(IdleValuesState())
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
  
  struct TrackingState {
    let properties: TrackedProperties<Subject>
    let observer: @Sendable () -> Void
    
    fileprivate init(
      properties: TrackedProperties<Subject>, 
      observer: @Sendable @escaping () -> Void
    ) {
      self.properties = properties
      self.observer = observer
    }
    
    func transitionToCancelled() -> State.Observation {
      return .cancelled
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservationRegistrar.State.Observation {
  init(idleChanges properties: TrackedProperties<Subject>) {
    self = .idleChanges(
      ObservationRegistrar.IdleChangesState(properties: properties))
  }
  
  init(idleValues properties: TrackedProperties<Subject>) {
    self = .idleValues(ObservationRegistrar.IdleValuesState())
  }
  
  init(
    tracking properties: TrackedProperties<Subject>, 
    _ observer: @Sendable @escaping () -> Void
  ) {
    self = .tracking(
      ObservationRegistrar.TrackingState(properties: properties, observer: observer))
  }
}
