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

import Observation
import _Concurrency

/// An asychronous sequence generated from a closure that tracks the transactional changes of `@Observable` types.
///
/// `Observed` conforms to `AsyncSequence`, providing a intutive and safe mechanism to track changes to
/// types that are marked as `@Observable` by using Swift Concurrency to indicate transactional boundaries
/// starting from the willSet of the first mutation to the next suspension point of the safe access.
@available(SwiftStdlib 9999, *)
public struct Observed<Element: Sendable, Failure: Error>: AsyncSequence, Sendable {
  /// An indication type for specifying if an element is the next element or a finishing of iteration
  ///
  /// This is used in conjunction with `Observed.untilFinished` to emit values until a `.finish` is 
  /// returned. All other elements in the observation emit closure for `untilFinished` should return
  /// `.next(element)` to indicate a properly formed next element from the observation closure.
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
    var id = 0
    var tracking = false
    var continuations: [Int: Continuation] = [:]
    
    // create a generation id for the unique identification of the continuations
    // this allows the shared awaiting of the willSets.
    // Most likely, there wont be more than a handful of active iterations
    // so this only needs to be unique for those active iterations
    // that are in the process of calling next.
    static func generation(_ state: _ManagedCriticalState<State>) -> Int {
      state.withCriticalRegion { state in
        defer { state.id &+= 1 }
        return state.id
      }
    }
    
    // the cancellation of awaiting on willSet only ferries in resuming early
    // it is the responsability of the caller to check if the task is actually
    // cancelled after awaiting the willSet to act accordingly.
    static func cancel(_ state: _ManagedCriticalState<State>, id: Int) {
      state.withCriticalRegion { state in
        guard let continuation = state.continuations.removeValue(forKey: id) else {
          // if there was no continuation yet active (e.g. it was cancelled at
          // the start of the invocation, then put a tombstone in to gate that
          // resuming later
          state.continuations[id] = .cancelled
          return nil as Continuation?
        }
        return continuation
      }?.resume()
    }
    
    // this atomically transitions the observation from a not yet tracked state
    // to a tracked state. No backwards transitions exist.
    static func startTracking(_ state: _ManagedCriticalState<State>) -> Bool {
      state.withCriticalRegion { state in
        if !state.tracking {
          state.tracking = true
          return true
        } else {
          return false
        }
      }
    }
    
    // fire off ALL awaiting willChange continuations such that they are no
    // longer pending.
    static func emitWillChange(_ state: _ManagedCriticalState<State>) {
      let continuations = state.withCriticalRegion { state in
        defer {
          state.continuations.removeAll()
        }
        return state.continuations.values
      }
      for continuation in continuations {
        continuation.resume()
      }
    }
    
    // install a willChange continuation into the set of continuations
    // this must take a locally unique id (to the active calls of next)
    static func willChange(_ state: _ManagedCriticalState<State>, id: Int) async {
      return await withUnsafeContinuation { continuation in
        state.withCriticalRegion { state in
          // first check if a cancelled tombstone exists, remove it,
          // and then return the freshly minted continuation to
          // be immediately resumed
          if case .cancelled = state.continuations[id] {
            state.continuations[id] = nil
            return continuation as UnsafeContinuation<Void, Never>?
          } else {
            state.continuations[id] = .active(continuation)
            return nil as UnsafeContinuation<Void, Never>?
          }
        }?.resume()
      }
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
  
  // internal funnel method for initialziation
  internal init(emit: Emit) {
    self.emit = emit
    self.state = _ManagedCriticalState(State())
  }
  
  /// Constructs an asynchronous sequence for a given closure by tracking changes of `@Observable` types.
  ///
  /// The emit closure is responsible for extracting a value out of a single or many `@Observable` types. When
  /// this initializer is invoked the closure inherits the current actor isolation and subseqent calls made 
  /// internally by `Observed` re-invoke the closure on that isolation if present. This means that if the 
  /// `Observed` is constructed on the `@MainActor`, all following calls to the emit closure will also be 
  /// isolated to the `@MainActor` and likewise for other isolations.
  ///
  /// In the case that this method is used in a `nonisolated` context it then means that the usage point
  /// must maintain rules pertaining to the `Sendable` nature of captured types. This method and other 
  /// parts of `Observed` do not add additional concurrency protection for these cases; so types must 
  /// be safe to maintain the safe construction and usage of `Observed` when called in an explicitly 
  /// `nonisolated` isolation domain.
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
  /// continues to be invoked until the .finished option is returned or an error is thrown. Additionally the emit
  /// closure follows the same isolation rules as the initializer form; where isolation is preserved from the 
  /// initial invocation and restored if present to ensure the closure is always isolated to the initial construction
  /// isolation domain.
  ///
  /// - Parameters:
  ///     - emit: A closure to generate an element for the sequence.
  public static func untilFinished(
    @_inheritActorContext _ emit: @escaping @isolated(any) @Sendable () throws(Failure) -> Iteration
  ) -> Observed<Element, Failure> {
    .init(emit: .iteration(emit))
  }
  
  public struct Iterator: AsyncIteratorProtocol {
    // the state ivar serves two purposes:
    // 1) to store a critical region of state of the mutations
    // 2) to idenitify the termination of _this_ sequence
    var state: _ManagedCriticalState<State>?
    let emit: Emit
    
    // this is the primary implementation of the tracking
    // it is bound to be called on the specified isolation of the construction
    fileprivate static func trackEmission(isolation trackingIsolation: isolated (any Actor)?, state: _ManagedCriticalState<State>, emit: Emit) throws(Failure) -> Iteration {
      // this ferries in an intermediate form with Result to skip over `withObservationTracking` not handling errors being thrown
      // particularly this case is that the error is also an iteration state transition data point (it terminates the sequence)
      // so we need to hold that to get a chance to catch and clean-up
      let result = withObservationTracking {
        switch emit {
        case .element(let element):
          Result(catching: element).map { Iteration.next($0) }
        case .iteration(let iteration):
          Result(catching: iteration)
        }
      } onChange: { [state] in
        // resume all cases where the awaiting continuations are awaiting a willSet
        State.emitWillChange(state)
      }
      return try result.get()
    }
    
    fileprivate mutating func terminate(throwing failure: Failure? = nil, id: Int) throws(Failure) -> Element? {
      // this is purely defensive to any leaking out of iteration generation ids
      state?.withCriticalRegion { state in
        state.continuations.removeValue(forKey: id)
      }?.resume()
      // flag the sequence as terminal by nil'ing out the state
      state = nil
      if let failure {
        throw failure
      } else {
        return nil
      }
    }
    
    fileprivate mutating func trackEmission(isolation iterationIsolation: isolated (any Actor)?, state: _ManagedCriticalState<State>, id: Int) async throws(Failure) -> Element? {
      guard !Task.isCancelled else {
        // the task was cancelled while awaiting a willChange so ensure a proper termination
        return try terminate(id: id)
      }
      // start by directly tracking the emission via a withObservation tracking on the isolation specified fro mthe init
      switch try await Iterator.trackEmission(isolation: emit.isolation, state: state, emit: emit) {
      case .finish: return try terminate(id: id)
      case .next(let element): return element
      }
    }
    
    public mutating func next(isolation iterationIsolation: isolated (any Actor)? = #isolation) async throws(Failure) -> Element? {
      // early exit if the sequence is terminal already
      guard let state else { return nil }
      // set up an id for this generation
      let id = State.generation(state)
      do {
        // there are two versions;
        // either the tracking has never yet started at all and we need to prime the pump
        // or the tracking has already started and we are going to await a change
        if State.startTracking(state) {
          return try await trackEmission(isolation: iterationIsolation, state: state, id: id)
        } else {
          
          // wait for the willChange (and NOT the value itself)
          // since this is going to be on the isolation of the object (e.g. the isolation specified in the initialization)
          // this will mean our next await for the emission will ensure the suspension return of the willChange context
          // back to the trailing edges of the mutations. In short, this enables the transactionality bounded by the
          // isolation of the mutation.
          await withTaskCancellationHandler {
            await State.willChange(state, id: id)
          } onCancel: {
            // ensure to clean out our continuation uon cancellation
            State.cancel(state, id: id)
          }
          return try await trackEmission(isolation: iterationIsolation, state: state, id: id)
        }
      } catch {
        // the user threw a failure in the closure so propigate that outwards and terminate the sequence
        return try terminate(throwing: error, id: id)
      }
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(state: state, emit: emit)
  }
}
