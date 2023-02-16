//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _ObservationRuntime

@available(SwiftStdlib 5.9, *)
public struct ObservationRegistar<Subject: Observable>: @unchecked Sendable {
  fileprivate
  struct Entry: Hashable {
    let members: MemberKeyPaths<Subject>
    let token: ObservationToken
    
    let _will: (inout ObservationTransaction<Subject>, Subject, UnsafeRawPointer, UnsafeRawPointer) -> Bool
    let _did: (inout ObservationTransaction<Subject>, Subject, UnsafeRawPointer) -> Bool
    let _commit: (inout ObservationTransaction<Subject>, Subject) -> Bool
    let _invalidate: () -> Void
    
    func hash(into hasher: inout Hasher) {
      hasher.combine(token)
    }
    
    static func == (_ lhs: Entry, _ rhs: Entry) -> Bool {
      return lhs.token == rhs.token
    }
    
    init<O: Observer, Model: ObservationTransactionModel>(observer: O, members: MemberKeyPaths<Subject>, token: ObservationToken, model: Model) where O.Subject == Subject {
      self.members = members
      self.token = token
      _will = { transaction, observable, raw, newValuePtr in
        let keyPath = Unmanaged<PartialKeyPath<Subject>>.fromOpaque(raw).takeUnretainedValue()
        func apply<Member>(_ type: Member.Type) -> Bool {
          model.register(transaction: &transaction, observer: observer, observable: observable, willSet: keyPath as! KeyPath<Subject, Member>, to: newValuePtr.assumingMemoryBound(to: Member.self).pointee)
        }
        return _openExistential(type(of: keyPath).valueType, do: apply)
      }
      
      _did = { transaction, observable, raw in
        let keyPath = Unmanaged<PartialKeyPath<Subject>>.fromOpaque(raw).takeUnretainedValue()
        func apply<Member>(_ type: Member.Type) -> Bool {
          model.register(transaction: &transaction, observer: observer, observable: observable, didSet: keyPath as! KeyPath<Subject, Member>)
        }
        return _openExistential(type(of: keyPath).valueType, do: apply)
      }
      _commit = model.commit(transaction:observable:)
      _invalidate = model.invalidate
    }
    
    @_effects(releasenone)
    func register<Member>(transaction: inout ObservationTransaction<Subject>, observable: Subject, willSet keyPath: UnsafeRawPointer, to newValue: Member) -> Bool {
      withUnsafePointer(to: newValue) { ptr in
        _will(&transaction, observable, keyPath, UnsafeRawPointer(ptr))
      }
    }
    
    @_effects(releasenone)
    func register(transaction: inout ObservationTransaction<Subject>, observable: Subject, didSet keyPath: UnsafeRawPointer) -> Bool {
      _did(&transaction, observable, keyPath)
    }
    
    @_effects(releasenone)
    func commit(transaction: inout ObservationTransaction<Subject>, observable: Subject) -> Bool {
      _commit(&transaction, observable)
    }
    
    @_effects(releasenone)
    func invalidate() {
      _invalidate()
    }
  }
  
  struct State: Deinitializable {
    // Registered observer + transaction model by token
    fileprivate var observers = [ObservationToken : Entry]()
    // Key path to entry back references for fast lookup
    fileprivate var registrations = [UnsafeRawPointer: Set<Entry>]()
    
    func deinitialize() {
      for entry in observers.values {
        entry.invalidate()
      }
    }
  }
  
  let state = ManagedCriticalState(managing: State())
  
  public init() { }
  
  public func addObserver<Model: ObservationTransactionModel>(_ observer: some Observer<Subject>, for members: MemberKeyPaths<Subject>, using transactionModel: Model) -> ObservationToken {
    let token = ObservationToken()
    state.withCriticalRegion { state in
      let entry = Entry(observer: observer, members: members, token: token, model: transactionModel)
      state.observers[token] = entry
      for keyPath in members.raw {
        state.registrations[keyPath, default: []].insert(entry)
      }
    }
    return token
  }
  
  public func removeObserver(_ observation: ObservationToken) {
    let entry = state.withCriticalRegion { state -> Any? in
      guard let entry = state.observers.removeValue(forKey: observation) else {
        return nil
      }
      for keyPath in entry.members.raw {
        state.registrations[keyPath]?.remove(entry)
        if state.registrations[keyPath]?.isEmpty == true {
          state.registrations.removeValue(forKey: keyPath)
        }
      }
      return entry
    }
    _fixLifetime(entry)
  }
  
  func remove(_ tokens: Set<ObservationToken>) {
    if tokens.isEmpty { return }
    state.withCriticalRegion { state in
      for token in tokens {
        if let entry = state.observers.removeValue(forKey: token) {
          for keyPath in entry.members.raw {
            state.registrations[keyPath]?.remove(entry)
          }
        }
      }
    }
  }
  
  public func beginAccess(_ observable: Subject) {
    if ThreadLocal[.transactionKey]?.assumingMemoryBound(to: ObservationTransaction<Subject>.self) == nil {
      let ptr = UnsafeMutablePointer<ObservationTransaction<Subject>>.allocate(capacity: 1)
      ptr.initialize(to: ObservationTransaction<Subject>(allocated: true))
      ThreadLocal[.transactionKey] = UnsafeMutableRawPointer(ptr)
    }
  }
  
  public func endAccess(_ observable: Subject) {
    if let container = ThreadLocal[.transactionKey]?.assumingMemoryBound(to: ObservationTransaction<Subject>.self) {
      container.pointee.notifyObservers(observable)
      if container.pointee.allocated == true {
        container.deinitialize(count: 1)
        container.deallocate()
      }
      ThreadLocal[.transactionKey] = nil
    }
  }
  
  public func withAccess<T>(_ observable: Subject, keyPath: PartialKeyPath<Subject>, _ apply: () throws -> T) rethrows -> T {
    ObservationTracking.registerAccess(propertyKeyPath: keyPath, subject: observable)
    return try apply()
  }
  
  func _withMutation<T, Member>(_ observable: Subject, raw: UnsafeRawPointer, to newValue: Member, container: UnsafeMutablePointer<ObservationTransaction<Subject>>, topLevel: Bool, _ apply: () throws -> T) rethrows -> T {
    let entries = state.withCriticalRegion { state in
      state.registrations[raw] ?? Set()
    }
    if entries.isEmpty {
      return try apply()
    } else {
      container.pointee.members._insert(raw)
      var toRemove = Set<ObservationToken>(minimumCapacity: entries.count)
    
      for entry in entries {
        if !entry.register(transaction: &container.pointee, observable: observable, willSet: raw, to: newValue) {
          toRemove.insert(entry.token)
        }
      }
      
      let result = try apply()
      
      for entry in entries {
        if toRemove.contains(entry.token) { continue }
        if !entry.register(transaction: &container.pointee, observable: observable, didSet: raw) {
          toRemove.insert(entry.token)
        } else if topLevel {
          if !entry.commit(transaction: &container.pointee, observable: observable) {
            toRemove.insert(entry.token)
          }
        }
      }
      _fixLifetime(entries)
      remove(toRemove)
      return result
    }
  }

  public func withMutation<T, Member>(_ observable: Subject, keyPath: __owned KeyPath<Subject, Member>, to newValue: Member, _ apply: () throws -> T) rethrows -> T {
    ObservationTracking.registerAccess(propertyKeyPath: keyPath, subject: observable)
    let raw = Unmanaged.passUnretained(keyPath).toOpaque()
    if let container = ThreadLocal[.transactionKey]?.assumingMemoryBound(to: ObservationTransaction<Subject>.self) {
      return try _withMutation(observable, raw: raw, to: newValue, container: container, topLevel: false, apply)
    } else {
      var container = ObservationTransaction<Subject>()
      return try withUnsafeMutablePointer(to: &container) { ptr in
        ThreadLocal[.transactionKey] = UnsafeMutableRawPointer(ptr)
        defer {
          ThreadLocal[.transactionKey] = nil
        }
        return try _withMutation(observable, raw: raw, to: newValue, container: ptr, topLevel: true, apply)
      }
    }
  }
  
  public func register<Member>(
    observable: Subject,
    willSet keyPath: KeyPath<Subject, Member>,
    to newValue: Member
  ) {
    let raw = Unmanaged.passUnretained(keyPath).toOpaque()
    if let container = ThreadLocal[.transactionKey]?.assumingMemoryBound(to: ObservationTransaction<Subject>.self) {
      let entries = state.withCriticalRegion { state in
        return state.registrations[raw] ?? Set()
      }
      if entries.isEmpty {
        return
      }
      container.pointee.members.insert(keyPath)
      var toRemove = Set<ObservationToken>()
      for entry in entries {
        if !entry.register(transaction: &container.pointee, observable: observable, willSet: raw, to: newValue) {
          toRemove.insert(entry.token)
        }
      }
      remove(toRemove)
    }
  }
  
  public func register<Member>(
    observable: Subject,
    didSet keyPath: KeyPath<Subject, Member>
  ) {
    let raw = Unmanaged.passUnretained(keyPath).toOpaque()
    if let container = ThreadLocal[.transactionKey]?.assumingMemoryBound(to: ObservationTransaction<Subject>.self) {
      container.pointee.members.insert(keyPath)
      let entries = state.withCriticalRegion { state in
        return state.registrations[raw] ?? Set()
      }
      if entries.isEmpty {
        return
      }
      container.pointee.members.insert(keyPath)
      var toRemove = Set<ObservationToken>()
      for entry in entries {
        if !entry.register(transaction: &container.pointee, observable: observable, didSet: raw) {
          toRemove.insert(entry.token)
        }
      }
      remove(toRemove)
    }
  }
}
