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

@available(SwiftStdlib 5.9, *)
public struct ObservedValues<Element> {
  private class Storage: ObservationRegistrar.ValueObservationStorage {
    func next() async -> Element? {
      return nil
    }
  }
  
  private final class StoredPropertyStorage: Storage {
    private struct State {
      private var terminal = false
      private var buffer = [Element]()
      private var continuations = [UnsafeContinuation<Element?, Never>]()
      
      @discardableResult
      private mutating func drain() -> Bool {
        if !continuations.isEmpty && !buffer.isEmpty {
          let continuation = continuations.removeFirst()
          let element = buffer.removeFirst()
          continuation.resume(returning: element)
        }
        if terminal && buffer.isEmpty && !continuations.isEmpty {
          let continuations = self.continuations
          self.continuations.removeAll(keepingCapacity: false)
          for continuation in continuations {
            continuation.resume(returning: nil)
          }
          return true
        }
        return false
      }
      
      internal mutating func emit(_ value: Element) -> Bool {
        if terminal && buffer.isEmpty {
          drain()
          return true
        }
        buffer.append(value)
        return drain()
      }
      
      internal mutating func next(_ continuation: UnsafeContinuation<Element?, Never>) {
        continuations.append(continuation)
        drain()
      }
      
      internal mutating func cancel() {
        terminal = true
        drain()
      }
    }
    
    private var id: Int = 0
    private let state = _ManagedCriticalState(State())
    
    internal init<Subject: Observable>(context: ObservationRegistrar.Context, subject: Subject, keyPath: KeyPath<Subject, Element>) {
      super.init()
      self.id = context.registerValues(for: [keyPath], storage: self)
    }
    
    internal override func next() async -> Element? {
      await withUnsafeContinuation { continuation in
        state.withCriticalRegion { $0.next(continuation) }
      }
    }
    
    internal override func emit<E>(_ element: E) -> Bool {
      state.withCriticalRegion { $0.emit(element as! Element) }
    }

    internal override func cancel() {
      state.withCriticalRegion { $0.cancel() }
    }
  }
  
  private final class ComputedPropertyStorage<Subject: Observable>: Storage {
    private struct State {
      private var observers = [ObjectIdentifier: Int]()
      private var terminal = false
      private var buffer = [Element]()
      private var continuations = [UnsafeContinuation<Element?, Never>]()
      
      private mutating func drain() {
        if !continuations.isEmpty && !buffer.isEmpty {
          let continuation = continuations.removeFirst()
          let element = buffer.removeFirst()
          continuation.resume(returning: element)
        }
        if terminal && buffer.isEmpty && !continuations.isEmpty {
          let continuations = self.continuations
          self.continuations.removeAll(keepingCapacity: false)
          for continuation in continuations {
            continuation.resume(returning: nil)
          }
        }
      }
      
      internal mutating func emit(_ value: Element) {
        if terminal && buffer.isEmpty {
          drain()
          return
        }
        buffer.append(value)
        drain()
      }
      
      internal mutating func next(_ continuation: UnsafeContinuation<Element?, Never>) {
        continuations.append(continuation)
        drain()
      }
      
      internal mutating func cancel() {
        terminal = true
        drain()
      }
    }

    private let keyPath: KeyPath<Subject, Element>
    private let state = _ManagedCriticalState(State())
    
    internal init(subject: Subject, keyPath: KeyPath<Subject, Element>) {
      self.keyPath = keyPath
      super.init()
      emitAccess(subject)
      
    }
    
    internal override func emit<E>(_ element: E) -> Bool {
      return true
    }
    
    private func emitAccess(_ subject: Subject) {
      let value = access(subject)
      state.withCriticalRegion { $0.emit(value) }
    }
    
    internal override func next() async -> Element? {
      await withUnsafeContinuation { continuation in
        state.withCriticalRegion { $0.next(continuation) }
      }
    }
    
    internal override func cancel() {
      state.withCriticalRegion { $0.cancel() }
    }
    
    private func access(_ subject: Subject) -> Element {
      var accessList: ObservationTracking._AccessList?
      let result = withUnsafeMutablePointer(to: &accessList) { ptr in
        let previous = _ThreadLocal.value
        _ThreadLocal.value = UnsafeMutableRawPointer(ptr)
        defer {
          if let scoped = ptr.pointee, let previous {
            if var prevList = previous.assumingMemoryBound(to: ObservationTracking._AccessList?.self).pointee {
              prevList.merge(scoped)
              previous.assumingMemoryBound(to: ObservationTracking._AccessList?.self).pointee = prevList
            } else {
              previous.assumingMemoryBound(to: ObservationTracking._AccessList?.self).pointee = scoped
            }
          }
          _ThreadLocal.value = previous
        }
        return subject[keyPath: keyPath]
      }
      if let list = accessList {
        let values = list.entries.mapValues { $0.addObserver { [state] subject in
          let values = state.withCriticalRegion { $0.observers }
          for (id, token) in values {
            list.entries[id]?.removeObserver(token)
          }
          self.emitAccess(subject as! Subject)
        }}
        state.withCriticalRegion { $0.observers = values }
      }
      return result
    }
  }
  
  private let storage: Storage
  
  public init<Subject: Observable>(of subject: Subject, computed keyPath: KeyPath<Subject, Element>) {
    storage = ComputedPropertyStorage(subject: subject, keyPath: keyPath)
  }
  
  public init<Subject: Observable>(of subject: Subject, stored keyPath: KeyPath<Subject, Element>, registrar: ObservationRegistrar) {
    storage = StoredPropertyStorage(context: registrar.context, subject: subject, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedValues: AsyncSequence {
  public struct Iterator: AsyncIteratorProtocol {
    let storage: Storage
    
    init(storage: Storage) {
      self.storage = storage
    }
    
    public mutating func next() async -> Element? {
      await storage.next()
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(storage: storage)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedValues: @unchecked Sendable where Element: Sendable { }

@available(*, unavailable)
extension ObservedValues.Iterator: Sendable { }
