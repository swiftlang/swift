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

#if STANDALONE
import Observation
#endif
import Synchronization
import _Concurrency

/// An asychronous sequence generated from a closure that tracks the transactional changes of `@Observable` types.
///
/// `Observed` conforms to `AsyncSequence`, providing a intutive and safe mechanism to track changes to
/// types that are marked as `@Observable` by using Swift Concurrency to indicate transactional boundaries
/// starting from the willSet of the first mutation to the next suspension point of the safe access.
#if !STANDALONE
@available(SwiftStdlib 9999, *)
#endif
public struct Observed<Element: Sendable, Failure: Error>: AsyncSequence, Sendable {
  final class SharedState<State>: Sendable {
    let criticalRegion: Mutex<State>
    
    init(_ state: consuming sending State) {
      criticalRegion = Mutex(state)
    }
    
    internal func withCriticalRegion<R, F: Error>(body: (inout sending State) throws(F) -> sending R) throws(F) -> R {
      try criticalRegion.withLock(body)
    }
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
    static func generation(_ state: SharedState<State>) -> Int {
      state.withCriticalRegion { state in
        defer { state.id &+= 1 }
        return state.id
      }
    }
    
    // the cancellation of awaiting on willSet only ferries in resuming early
    // it is the responsability of the caller to check if the task is actually
    // cancelled after awaiting the willSet to act accordingly.
    static func cancel(_ state: SharedState<State>, id: Int) {
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
    static func startTracking(_ state: SharedState<State>) -> Bool {
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
    static func emitWillChange(_ state: SharedState<State>) {
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
    static func willChange(_ state: SharedState<State>, id: Int) async {
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
  
  let state: SharedState<State>
  let emit: @isolated(any) @Sendable () throws(Failure) -> Element?
  
  /// Constructs an asynchronous sequence for a given closure by tracking changes of `@Observable` types.
  ///
  /// The emit closure is responsible for extracting a value out of a single or many `@Observable` types. When `nil` is
  /// returned the sequence terminates and no more values will be produced. Likewise when an error is thrown, iterations will
  /// catch that error and the sequence will be in a terminal state, no longer producing values.
  ///
  /// When constructing an `Observed` asynchronous sequence it is strongly suggested that the isolation parameter is left
  /// to the default value. Other values specified are only for advanced and unique use cases and may result in unexpected
  /// behavior.
  ///
  /// - Parameters:
  ///     - isolation:  The concurrency isolation domain of the caller.
  ///     - emit: A closure to generate an element for the sequence.
  ///
  public init(
    @_inheritActorContext _ emit: @escaping @isolated(any) @Sendable () throws(Failure) -> Element?
  ) {
    self.emit = emit
    self.state = SharedState(State())
  }
  
  public struct Iterator: AsyncIteratorProtocol {
    // the state ivar serves two purposes:
    // 1) to store a critical region of state of the mutations
    // 2) to idenitify the termination of _this_ sequence
    var state: SharedState<State>?
    let emit: @isolated(any) @Sendable () throws(Failure) -> Element?
    
    // this is the primary implementation of the tracking
    // it is bound to be called on the specified isolation of the construction
    fileprivate static func trackEmission(isolation trackingIsolation: isolated (any Actor)?, state: SharedState<State>, emit: @escaping @isolated(any) @Sendable () throws(Failure) -> Element?) throws(Failure) -> Element? {
      // this ferries in an intermediate form with Result to skip over `withObservationTracking` not handling errors being thrown
      // particularly this case is that the error is also an iteration state transition data point (it terminates the sequence)
      // so we need to hold that to get a chance to catch and clean-up
      let result = withObservationTracking {
        Result(catching: emit)
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
    
    fileprivate mutating func trackEmission(isolation iterationIsolation: isolated (any Actor)?, state: SharedState<State>, id: Int) async throws(Failure) -> Element? {
      guard !Task.isCancelled else {
        // the task was cancelled while awaiting a willChange so ensure a proper termination
        return try terminate(id: id)
      }
      // start by directly tracking the emission via a withObservation tracking on the isolation specified fro mthe init
      guard let element = try await Iterator.trackEmission(isolation: emit.isolation, state: state, emit: emit) else {
        // the user returned a nil from the closure so terminate the sequence
        return try terminate(id: id)
      }
      return element
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
