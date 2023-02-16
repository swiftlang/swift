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
import _Concurrency

@available(SwiftStdlib 5.9, *)
extension Observable {
  public func changes<Member: Sendable>(for keyPath: KeyPath<Self, Member>) -> ObservedChanges<Self, Member> {
    ObservedChanges(self, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
public struct ObservedChanges<Subject: Observable, Member: Sendable> {
  let subject: Subject
  let keyPath: KeyPath<Subject, Member>
  
  init(_ subject: Subject, keyPath: KeyPath<Subject, Member>) {
    self.subject = subject
    self.keyPath = keyPath
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedChanges: AsyncSequence {
  public typealias Element = Member
  
  public struct Iterator: AsyncIteratorProtocol {
    struct Iteration: Sendable {
      enum State {
        case idle
        case awaiting(UnsafeContinuation<Element?, Never>)
        case pending([Element], Bool)
        case terminal
      }
      
      var state: State = .idle
      var pending: Element?
      
      var terminal: Bool {
        switch state {
        case .terminal: return true
        default: return false
        }
      }
      
      mutating func next(_ continuation: UnsafeContinuation<Element?, Never>) {
        switch state {
        case .idle:
          state = .awaiting(continuation)
        case .awaiting:
          fatalError()
        case .pending(var elements, let terminal):
          let element = elements.removeFirst()
          if elements.isEmpty {
            state = terminal ? .terminal : .idle
          } else {
            state = .pending(elements, terminal)
          }
          continuation.resume(returning: element)
        case .terminal:
          continuation.resume(returning: nil)
        }
      }
      
      mutating func emit(_ member: Element) {
        switch state {
        case .idle:
          state = .pending([member], false)
        case .awaiting(let continuation):
          state = .idle
          continuation.resume(returning: member)
        case .pending(var elements, let terminal):
          elements.append(member)
          state = .pending(elements, terminal)
        case .terminal:
          break
        }
      }
      
      mutating func terminate() {
        switch state {
        case .idle:
          state = .terminal
        case .awaiting(let continuation):
          state = .terminal
          continuation.resume(returning: nil)
        case .pending(let elements, _):
          state = .pending(elements, true)
        case .terminal:
          break
        }
      }
    }
    
    struct IterationTransactionModel: ObservationTransactionModel {
      func register<O, Member>(transaction: inout ObservationTransaction<O.Subject>, observer: O, observable: O.Subject, willSet keyPath: KeyPath<O.Subject, Member>, to newValue: Member) -> Bool where O : Observer {
        return true
      }
      
      func register<O, Member>(transaction: inout ObservationTransaction<O.Subject>, observer: O, observable: O.Subject, didSet keyPath: KeyPath<O.Subject, Member>) -> Bool where O : Observer {
        return true
      }
      
      func commit<S>(transaction: inout ObservationTransaction<S>, observable: S) -> Bool where Subject : Observable {
        let value = (observable as! Subject)[keyPath: keyPath]
        return state.withCriticalRegion { state in
          state.emit(value)
          return !state.terminal
        }
      }
      
      let state: ManagedCriticalState<Iteration>
      let keyPath: KeyPath<Subject, Member>
      
      func invalidate() {
        state.withCriticalRegion { state in
          state.terminate()
        }
      }
    }
    
    struct IterationObserver: Observer {
      func changes(_ subject: Subject, to members: MemberKeyPaths<Subject>) { }
    }
    
    enum State {
      case initial(Subject, KeyPath<Subject, Member>)
      case active(ManagedCriticalState<Iteration>)
      case terminal
    }
    
    var state: State
    
    init(_ subject: Subject, keyPath: KeyPath<Subject, Member>) {
      state = .initial(subject, keyPath)
    }
    
    public mutating func next() async -> Element? {
      switch state {
      case .initial(let subject, let keyPath):
        let iterationState = ManagedCriticalState(Iteration())
        _ = subject.addObserver(IterationObserver(), for: [keyPath], using: IterationTransactionModel(state: iterationState, keyPath: keyPath))
        state = .active(iterationState)
        return await next()
      case .active(let managedCriticalState):
        let element = await withTaskCancellationHandler {
          await withUnsafeContinuation { continuation in
            managedCriticalState.withCriticalRegion { state in
              state.next(continuation)
            }
          }
        } onCancel: {
          managedCriticalState.withCriticalRegion { state in
            state.terminate()
          }
        }
        if element == nil {
          state = .terminal
        }
        return element
      case .terminal:
        return nil
      }
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(subject, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedChanges: @unchecked Sendable where Subject: Sendable { }

@available(*, unavailable)
extension ObservedChanges.Iterator: Sendable { }
