//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import _Concurrency

/// An asynchronous sequence generated from a closure that tracks the transactional changes of `@Observable` types.
///
/// `Observations` conforms to `AsyncSequence`, providing an intuitive and safe mechanism to track changes to
/// types that are marked as `@Observable` by using Swift Concurrency to indicate transactional boundaries
/// starting from the willSet of the first mutation to the next suspension point of the safe access.
@available(SwiftStdlib 6.2, *)
public struct Observations<Element: Sendable, Failure: Error>: AsyncSequence, Sendable {
  public enum Iteration: Sendable {
    case next(Element)
    case finish
  }
  
  struct State {
    enum Continuation {
      case cancelled
      case active(UnsafeContinuation<Void, Never>)
      func resume() {
        switch self {
        case .cancelled: break
        case .active(let continuation): continuation.resume()
        }
      }
    }
    var nextIteratorID = 0
    var continuations: [Int: Continuation] = [:]
    var dirty: Set<Int> = []
    var activeIterators: Set<Int> = []

    static func makeIteratorID(_ state: _ManagedCriticalState<State>) -> Int {
      state.withCriticalRegion { state in
        defer { state.nextIteratorID &+= 1 }
        state.activeIterators.insert(state.nextIteratorID)
        return state.nextIteratorID
      }
    }

    // the cancellation of awaiting on willSet only ferries in resuming early
    // it is the responsibility of the caller to check if the task is actually
    // cancelled after awaiting the willSet to act accordingly.
    static func cancel(_ state: _ManagedCriticalState<State>, iteratorID: Int) {
      state.withCriticalRegion { state in
        guard let continuation = state.continuations.removeValue(forKey: iteratorID) else {
          // if there was no continuation yet active (e.g. it was cancelled at
          // the start of the invocation, then put a tombstone in to gate that
          // resuming later
          state.continuations[iteratorID] = .cancelled
          return nil as Continuation?
        }
        return continuation
      }?.resume()
    }

    // fire off ALL awaiting willChange continuations such that they are no
    // longer pending.
    static func emitWillChange(_ state: _ManagedCriticalState<State>) {
      let continuations = state.withCriticalRegion { state in
        // mark active iterators that don't have a continuation as dirty
        // only those specific iterators need to be woken on their next
        // willChange call — this avoids a shared dirty flag leaking across
        // iterator lifetimes.
        for iteratorID in state.activeIterators {
          if state.continuations[iteratorID] == nil {
            state.dirty.insert(iteratorID)
          }
        }
        defer {
          state.continuations.removeAll()
        }
        return state.continuations.values
      }
      for continuation in continuations {
        continuation.resume()
      }
    }

    // install a willChange continuation for a specific iterator
    static func willChange(isolation iterationIsolation: isolated (any Actor)? = #isolation, state: _ManagedCriticalState<State>, iteratorID: Int) async {
      return await withUnsafeContinuation { continuation in
        state.withCriticalRegion { state in
          switch state.continuations[iteratorID] {
          case .cancelled:
            return continuation as UnsafeContinuation<Void, Never>?
          case .active:
            // the Iterator itself cannot be shared across isolations so any call to next that may share an id is a misbehavior
            // or an internal book-keeping failure
            fatalError("Iterator incorrectly shared across task isolations")
          case .none:
            if state.dirty.remove(iteratorID) != nil {
              return continuation
            } else {
              state.continuations[iteratorID] = .active(continuation)
              return nil
            }
          }
        }?.resume()
      }
    }

    static func terminate(_ state: _ManagedCriticalState<State>, iteratorID: Int) {
      state.withCriticalRegion { state in
        state.activeIterators.remove(iteratorID)
        state.dirty.remove(iteratorID)
        return state.continuations.removeValue(forKey: iteratorID)
      }?.resume()
    }
  }
  
  // @isolated(any) closures cannot be composed and retain or forward their isolation
  // this basically would be replaced with `{ .next(elementProducer()) }` if that
  // were to become possible.
  enum Emit {
    case iteration(@isolated(any) @Sendable () throws(Failure) -> Iteration)
    case element(@isolated(any) @Sendable () throws(Failure) -> Element)
    
    var isolation: (any Actor)? {
      switch self {
      case .iteration(let closure): closure.isolation
      case .element(let closure): closure.isolation
      }
    }
  }
  
  let state: _ManagedCriticalState<State>
  let emit: Emit
  
  // internal funnel method for initialization
  internal init(emit: Emit) {
    self.emit = emit
    self.state = _ManagedCriticalState(State())
  }
  
  /// Constructs an asynchronous sequence for a given closure by tracking changes of `@Observable` types.
  ///
  /// The emit closure is responsible for extracting a value out of a single or many `@Observable` types.
  ///
  /// - Parameters:
  ///     - isolation:  The concurrency isolation domain of the caller.
  ///     - emit: A closure to generate an element for the sequence.
  public init(
    @_inheritActorContext _ emit: @escaping @isolated(any) @Sendable () throws(Failure) -> Element
  ) {
    self.init(emit: .element(emit))
  }
  
  /// Constructs an asynchronous sequence for a given closure by tracking changes of `@Observable` types.
  ///
  /// The emit closure is responsible for extracting a value out of a single or many `@Observable` types. This method
  /// continues to be invoked until the .finished option is returned or an error is thrown.
  ///
  /// - Parameters:
  ///     - isolation:  The concurrency isolation domain of the caller.
  ///     - emit: A closure to generate an element for the sequence.
  public static func untilFinished(
    @_inheritActorContext _ emit: @escaping @isolated(any) @Sendable () throws(Failure) -> Iteration
  ) -> Observations<Element, Failure> {
    .init(emit: .iteration(emit))
  }
  
  public struct Iterator: AsyncIteratorProtocol {
    let extent: Extent
    let emit: Emit
    
    var started = false

    final class Extent {
      var state: _ManagedCriticalState<State>?
      let iteratorID: Int

      init(state: _ManagedCriticalState<State>, iteratorID: Int) {
        self.state = state
        self.iteratorID = iteratorID
      }

      deinit {
        if let state {
          State.terminate(state, iteratorID: iteratorID)
        }
      }
    }

    init(state: _ManagedCriticalState<State>, emit: Emit, iteratorID: Int) {
      self.extent = Extent(state: state, iteratorID: iteratorID)
      self.emit = emit
    }

    fileprivate static func trackEmission(isolation trackingIsolation: isolated (any Actor)?, state: _ManagedCriticalState<State>, emit: Emit) throws(Failure) -> Iteration {
      if #available(SwiftStdlib 6.4, *) {
        return try withObservationTracking(options: [.willSet, .deinit]) { () throws(Failure) -> Observations<Element, Failure>.Iteration in
          switch emit {
          case .element(let element):
            let extracted: () throws(Failure) -> Element = element
            return try Iteration.next(extracted())
          case .iteration(let iteration):
            let extracted: () throws(Failure) -> Iteration = iteration
            return try extracted()
          }
        } onChange: { [state] (event) in
          // resume all cases where the awaiting continuations are awaiting a willSet
          State.emitWillChange(state)
        }
      } else {
        // fallback to the previous version
        let result = withObservationTracking { () -> Result<Observations<Element, Failure>.Iteration, Failure> in
          do {
            switch emit {
            case .element(let element):
              let extracted: () throws(Failure) -> Element = element
              return .success(try Iteration.next(extracted()))
            case .iteration(let iteration):
              let extracted: () throws(Failure) -> Iteration = iteration
              return .success(try extracted())
            }
          } catch {
            return .failure(error as! Failure)
          }
        } onChange: { [state] in
          // resume all cases where the awaiting continuations are awaiting a willSet
          State.emitWillChange(state)
        }
        switch result {
        case .success(let value): return value
        case .failure(let failure): throw failure
        }
      }
    }

    fileprivate mutating func terminate(throwing failure: Failure? = nil) throws(Failure) -> Element? {
      if let state = extent.state {
        State.terminate(state, iteratorID: extent.iteratorID)
      }
      // flag the sequence as terminal by nil'ing out the state
      extent.state = nil
      if let failure {
        throw failure
      } else {
        return nil
      }
    }

    fileprivate mutating func trackEmission(isolation iterationIsolation: isolated (any Actor)?, state: _ManagedCriticalState<State>) async throws(Failure) -> Element? {
      guard !Task.isCancelled else {
        return try terminate()
      }
      switch try await Iterator.trackEmission(isolation: emit.isolation, state: state, emit: emit) {
      case .finish: return try terminate()
      case .next(let element): return element
      }
    }

    public mutating func next(isolation iterationIsolation: isolated (any Actor)? = #isolation) async throws(Failure) -> Element? {
      guard let state = extent.state else { return nil }
      let iteratorID = extent.iteratorID
      do {
        // there are two versions;
        // either the tracking has never yet started at all and we need to prime the pump for this specific iterator
        // or the tracking has already started and we are going to await a change
        if !started {
          started = true
          return try await trackEmission(isolation: iterationIsolation, state: state)
        } else {
          await withTaskCancellationHandler(operation: {
            await State.willChange(isolation: iterationIsolation, state: state, iteratorID: iteratorID)
          }, onCancel: { [iteratorID] in
            State.cancel(state, iteratorID: iteratorID)
          })
          return try await trackEmission(isolation: iterationIsolation, state: state)
        }
      } catch {
        return try terminate(throwing: error)
      }
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(state: state, emit: emit, iteratorID: State.makeIteratorID(state))
  }
}
