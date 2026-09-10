//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
#if ASYNC_STREAM_STANDALONE
@_exported import _Concurrency
import Darwin

func _lockWordCount() -> Int {
  let sz =
    MemoryLayout<os_unfair_lock>.size / MemoryLayout<UnsafeRawPointer>.size
  return max(sz, 1)
}

func _lockInit(_ ptr: UnsafeRawPointer) {
  UnsafeMutableRawPointer(mutating: ptr)
    .assumingMemoryBound(to: os_unfair_lock.self)
    .initialize(to: os_unfair_lock())
}

func _lock(_ ptr: UnsafeRawPointer) {
  os_unfair_lock_lock(UnsafeMutableRawPointer(mutating: ptr)
    .assumingMemoryBound(to: os_unfair_lock.self))
}

func _unlock(_ ptr: UnsafeRawPointer) {
  os_unfair_lock_unlock(UnsafeMutableRawPointer(mutating: ptr)
    .assumingMemoryBound(to: os_unfair_lock.self))
}
#else
@_silgen_name("_swift_async_stream_lock_size")
func _lockWordCount() -> Int

@_silgen_name("_swift_async_stream_lock_init")
func _lockInit(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_async_stream_lock_lock")
func _lock(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_async_stream_lock_unlock")
func _unlock(_ ptr: UnsafeRawPointer)
#endif

fileprivate struct Disconnected<Value: ~Copyable>: ~Copyable, @unchecked Sendable {
  private var value: Value?

  private init() {
    self.value = nil
  }

  init(_ value: consuming sending Value) {
    self.value = consume value
  }

  mutating func take() -> sending Value {
    let oldValue = consume value
    self = .init()
    return oldValue!
  }
}

/// The state machine backing the continuation-based variant of `Async{Throwing}Stream`.
///
/// States:
///
///   - `idle`:  The stream is active with **no consumers present**,
///   and may accept new elements (depending on the `BufferingPolicy`).
///   - `waiting`: The stream is active with **at least one consumer present**,
///   and new elements are directly delivered to the next consumer.
///   - `draining`: The stream **no longer accepts new elements**,
///   new consumers drain the buffered elements.
///   - `terminating`: The stream is terminating,
///   and is currently running the termination handler, before moving on to terminated.
///   - `terminated`: The stream is in a terminal state,
///   **no new elements are accepted**, and **new consumers return immediately**.
///
/// Transitions:
///
/// ```text
/// Current State   Possible Next State
/// -------------   -------------------
/// idle          ->  idle, waiting, draining, terminating
/// waiting       ->  idle, waiting, terminating
/// draining      ->  draining, terminating
/// terminating   ->  terminated
/// terminated    ->  terminated
/// ```
///
/// Actions:
///
/// - `YieldAction`:
///   - `resume`:  The next consumer is resumed with the newly yielded value.
///   - `none`:  No action is taken.
///
/// - `NextAction`:
///   - `resume`: The new consumer is resumed.
///   - `throw`: The new consumer is resumed by throwing an error of type `Failure`.
///   - `suspend`:  The new consumer is enqueued. No action is taken.
///
/// - `TerminateAction`:
///   - `callAndResume`: The `TerminationHandler` is invoked, and all consumers are resumed afterward.
///   - `call`: Only the `TerminationHandler` is invoked.
///   - `none`: No action is taken.
///
/// Behavior:
/// The state machine is single-consumer–based. However, instead of crashing on concurrent iteration,
/// the consumer that “loses” the race to `next()` is enqueued in a **FIFO queue** and **eventually resumed**.
///
/// Furthermore, when the stream reaches its terminal state and an onTermination closure is set,
/// the closure is invoked **exactly once, after which it is cleared**.
///
/// Once the stream has reached its terminal state, all subsequent consumers will **immediately return nil**,
/// and any **new values are rejected**.
@safe
internal final class _AsyncStreamStorage<
  Element, Failure: Error, PublicTermination
>: @unchecked Sendable {
  struct Continuation {
    enum BufferingPolicy {
      case unbounded

      case bufferingOldest(Int)

      case bufferingNewest(Int)
    }

    enum YieldResult {
      case enqueued(remaining: Int)

      case dropped(Element)

      case terminated
    }

    enum Termination {
      case finished(Failure?)

      case cancelled
    }
  }

  @safe
  struct StateMachine: ~Copyable {
    typealias Buffer = _Deque<Element>
    typealias Consumer = UnsafeContinuation<Result<Element?, Failure>, Never> // TODO: Switch to ~Copyable Continuation
    typealias Consumers = _Deque<Consumer> // TODO: Switch to UniqueDeque
    typealias TerminationHandler = _AsyncStreamTerminationHandlerBox<Element, Failure, PublicTermination>

    @unsafe
    enum State: ~Copyable {
      struct Idle: ~Copyable {
        var buffer: Buffer
        let bufferingPolicy: Continuation.BufferingPolicy
        var terminationHandler: TerminationHandler?
      }

      @unsafe
      struct Waiting: ~Copyable {
        var consumers: Consumers
        let bufferingPolicy: Continuation.BufferingPolicy
        var terminationHandler: TerminationHandler?
      }

      struct Draining: ~Copyable {
        var buffer: Buffer
        var failure: Failure?
        var terminationHandler: TerminationHandler?
      }

      // A transient state entered on the cancellation path.
      // The stream is terminating but the outcome is not yet finalized:
      // the termination handler still has to run and may call `finish(throwing:)`
      // to set a failure.
      struct Terminating: ~Copyable {
        var buffer: Buffer
        private(set) var failure: Failure?
        var terminationHandler: TerminationHandler?

        /// Returns true if the value was set, false otherwise.
        @export(implementation)
        mutating func setFailureOnce(_ failure: Failure?) -> Bool {
          guard self.failure == nil else {
            return false
          }
          self.failure = failure
          return true
        }
        @export(implementation)
        mutating func takeFailure() -> Failure? {
          self.failure.take()
        }
      }

      struct Terminated: ~Copyable {
        var failure: Failure?
        var terminationHandler: TerminationHandler? // TODO: Remove this in a follow-up PR
      }

      case idle(Idle)

      case waiting(Waiting)

      case draining(Draining)

      case terminating(Terminating)

      case terminated(Terminated)
    }

    @unsafe
    enum YieldAction {
      case resume(
        consumer: Consumer,
        element: Element,
        yieldResult: Continuation.YieldResult
      )

      case none(yieldResult: Continuation.YieldResult)
    }

    @unsafe
    enum NextAction {
      case resume(
        consumer: Consumer,
        element: Element?
      )

      case `throw`(
        consumer: Consumer,
        failure: Failure
      )

      case suspend
    }

    @unsafe
    enum TerminateAction: ~Copyable {
      @unsafe
      struct CallAndResume: ~Copyable {
        var consumers: Consumers
        let terminationHandler: TerminationHandler?
      }

      case callAndResume(CallAndResume)

      case call(terminationHandler: TerminationHandler?)

      case none
    }

    private var state: State

    init(state: consuming State) {
      unsafe self.state = unsafe state
    }

    init(bufferingPolicy: Continuation.BufferingPolicy) {
      unsafe self.state = unsafe .idle(.init(
        buffer: [],
        bufferingPolicy: bufferingPolicy
      ))
    }
  }

  private let lock: UnsafeMutableRawPointer
  private var stateMachine: StateMachine

  init(bufferingPolicy: Continuation.BufferingPolicy) {
    unsafe self.lock = unsafe UnsafeMutableRawPointer.allocate(
      byteCount: _lockWordCount() * MemoryLayout<UnsafeRawPointer>.stride,
      alignment: MemoryLayout<UnsafeRawPointer>.alignment
    )
    unsafe _lockInit(self.lock)

    self.stateMachine = StateMachine(
      bufferingPolicy: bufferingPolicy
    )
  }

  deinit {
    self.terminate(.cancelled)
    unsafe self.lock.deallocate()
  }
}

extension _AsyncStreamStorage.StateMachine {
  enum BufferingNewestDecision {
    case append
    case dropOldestValue
    case dropNewValue

    init(bufferCount: Int, limit: Int) {
      if bufferCount < limit && limit > .zero {
        self = .append
      } else if bufferCount >= limit && limit > .zero {
        self = .dropOldestValue
      } else {
        self = .dropNewValue
      }
    }
  }

  func getOnTermination() -> TerminationHandler? {
    switch unsafe self.state { // TODO: Return a TerminationHandler only in certain states
    case .idle(let idle):
      return idle.terminationHandler

    case .waiting(let waiting):
      return unsafe waiting.terminationHandler

    case .draining(let draining):
      return draining.terminationHandler

    case .terminating(let terminating):
      return terminating.terminationHandler

    case .terminated(let terminated):
      return terminated.terminationHandler
    }
  }

  mutating func setOnTermination(_ newValue: TerminationHandler?) -> TerminationHandler? {
    let previous: TerminationHandler?

    switch unsafe consume self.state { // TODO: Set a TerminationHandler only in certain states
    case .idle(var idle):
      previous = idle.terminationHandler
      idle.terminationHandler = newValue
      unsafe self = .init(state: .idle(idle))

    case .waiting(var waiting):
      previous = unsafe waiting.terminationHandler
      unsafe waiting.terminationHandler = newValue
      unsafe self = .init(state: .waiting(waiting))

    case .draining(var draining):
      previous = draining.terminationHandler
      draining.terminationHandler = newValue
      unsafe self = .init(state: .draining(draining))

    case .terminating(var terminating):
      previous = terminating.terminationHandler
      terminating.terminationHandler = newValue
      unsafe self = .init(state: .terminating(terminating))

    case .terminated(var terminated):
      previous = terminated.terminationHandler
      terminated.terminationHandler = newValue
      unsafe self = .init(state: .terminated(terminated))
    }

    return previous
  }

  mutating func yield(_ value: consuming sending Element) -> YieldAction {
    switch unsafe consume self.state {
    case .idle(var idle):
      switch idle.bufferingPolicy {
      case .unbounded:
        idle.buffer.append(value)
        unsafe self = .init(state: .idle(idle))
        return unsafe .none(yieldResult: .enqueued(remaining: .max))

      case .bufferingOldest(let limit):
        if idle.buffer.count < limit {
          idle.buffer.append(value)
          let count = idle.buffer.count
          unsafe self = .init(state: .idle(idle))
          return unsafe .none(yieldResult: .enqueued(remaining: limit - count))

        } else {
          unsafe self = .init(state: .idle(idle))
          return unsafe .none(yieldResult: .dropped(value))
        }

      case .bufferingNewest(let limit):
        let decision = BufferingNewestDecision(
          bufferCount: idle.buffer.count,
          limit: limit
        )

        switch decision {
        case .append:
          idle.buffer.append(value)
          let count = idle.buffer.count
          unsafe self = .init(state: .idle(idle))
          return unsafe .none(yieldResult: .enqueued(remaining: limit - count))

        case .dropOldestValue:
          let droppedValue = idle.buffer.removeFirst()
          idle.buffer.append(value)
          unsafe self = .init(state: .idle(idle))
          return unsafe .none(yieldResult: .dropped(droppedValue))

        case .dropNewValue:
          unsafe self = .init(state: .idle(idle))
          return unsafe .none(yieldResult: .dropped(value))
        }
      }

    case .waiting(var waiting):
      let bufferingPolicy = unsafe waiting.bufferingPolicy
      let consumer = unsafe waiting.consumers.removeFirst()

      if unsafe waiting.consumers.isEmpty {
        unsafe self = .init(state: .idle(.init(
          buffer: [],
          bufferingPolicy: waiting.bufferingPolicy,
          terminationHandler: waiting.terminationHandler.take()
        )))

      } else {
        unsafe self = .init(state: .waiting(waiting))
      }

      switch bufferingPolicy {
      case .unbounded:
        return unsafe .resume(
          consumer: consumer,
          element: value,
          yieldResult: .enqueued(remaining: .max)
        )

      case .bufferingOldest(let limit), .bufferingNewest(let limit):
        return unsafe .resume(
          consumer: consumer,
          element: value,
          yieldResult: .enqueued(remaining: limit)
        )
      }

    case .draining(let draining):
      unsafe self = .init(state: .draining(draining))
      return unsafe .none(yieldResult: .terminated)

    case .terminating(let terminating):
      unsafe self = .init(state: .terminating(terminating))
      return unsafe .none(yieldResult: .terminated)

    case .terminated(let terminated):
      unsafe self = .init(state: .terminated(terminated))
      return unsafe .none(yieldResult: .terminated)
    }
  }

  mutating func next(_ consumer: consuming Consumer) -> NextAction {
    switch unsafe consume self.state {
    case .idle(var idle):
      if idle.buffer.isEmpty {
        unsafe self = .init(state: .waiting(.init(
          consumers: [consumer],
          bufferingPolicy: idle.bufferingPolicy,
          terminationHandler: idle.terminationHandler.take()
        )))
        return unsafe .suspend

      } else {
        let element = idle.buffer.removeFirst()
        unsafe self = .init(state: .idle(idle))
        return unsafe .resume(
          consumer: consumer,
          element: element
        )
      }

    case .waiting(var waiting):
      unsafe waiting.consumers.append(consumer)
      unsafe self = .init(state: .waiting(waiting))
      return unsafe .suspend

    case .draining(var draining):
      guard
        let element = draining.buffer.popFirst()
      else {
        unsafe self = .init(state: .terminated(.init(
          terminationHandler: draining.terminationHandler.take()
        )))

        switch draining.failure {
        case .some(let failure):
          return unsafe .throw(
            consumer: consumer,
            failure: failure
          )

        case .none:
          return unsafe .resume(
            consumer: consumer,
            element: nil
          )
        }
      }

      if draining.buffer.isEmpty {
        unsafe self = .init(state: .terminated(.init(
          failure: draining.failure,
          terminationHandler: draining.terminationHandler.take()
        )))
        return unsafe .resume(
          consumer: consumer,
          element: element
        )

      } else {
        unsafe self = .init(state: .draining(draining))
        return unsafe .resume(
          consumer: consumer,
          element: element
        )
      }

    case .terminating(var terminating):
      // The stream is terminating but the outcome is not yet final.
      // Deliver any remaining buffered elements just like `draining`; the failure thrown
      // once the buffer empties is finalized by the caller of `terminate` after
      // the handler returns
      guard let element = terminating.buffer.popFirst() else {
        unsafe self = .init(state: .terminated(.init(
          terminationHandler: terminating.terminationHandler.take()
        )))

        switch terminating.failure {
        case .some(let failure):
          return unsafe .throw(
            consumer: consumer,
            failure: failure
          )

        case .none:
          return unsafe .resume(
            consumer: consumer,
            element: nil
          )
        }
      }

      unsafe self = .init(state: .terminating(terminating))
      return unsafe .resume(
        consumer: consumer,
        element: element
      )

    case .terminated(let terminated):
      // Reaching the terminal state drops the termination handler, per the
      // documented `onTermination` contract: the handler is released once the
      // stream has terminated
      unsafe self = .init(state: .terminated(.init()))

      switch terminated.failure {
      case .some(let failure):
        return unsafe .throw(
          consumer: consumer,
          failure: failure
        )

      case .none:
        return unsafe .resume(
          consumer: consumer,
          element: nil
        )
      }
    }
  }

  /// Terminates the stream with a final outcome.
  mutating func terminate(
    _ failure: consuming Failure?
  ) -> TerminateAction {
    switch unsafe consume self.state {
    case .idle(var idle):
      if idle.buffer.isEmpty {
        unsafe self = .init(state: .terminated(.init(failure: failure)))
      } else {
        unsafe self = .init(state: .draining(.init(
          buffer: idle.buffer,
          failure: failure,
        )))
      }
      return unsafe .call(
        terminationHandler: idle.terminationHandler.take()
      )

    case .waiting(var waiting):
      unsafe self = .init(state: .terminated(.init(failure: failure)))
      return unsafe .callAndResume(.init(
        consumers: waiting.consumers,
        terminationHandler: waiting.terminationHandler.take(),
      ))

    case .draining(let draining):
      unsafe self = .init(state: .draining(draining))
      return unsafe .none

    case .terminating(var terminating):
      // A re-entrant `finish(throwing:)` from within the termination handler,
      // while the cancellation outcome is not yet final.
      _ = terminating.setFailureOnce(failure)
      unsafe self = .init(state: .terminating(terminating))
      return unsafe .none

    case .terminated(let terminated):
      // Already final: later terminations are ignored (first finish wins)
      unsafe self = .init(state: .terminated(terminated))
      return unsafe .none
    }
  }

  /// Begins terminating the stream on the cancellation path.
  ///
  /// Enters the transient `terminating` state so the termination handler can
  /// still supply a failure via `finish(throwing:)`.
  mutating func beginTerminating() -> TerminateAction {
    switch unsafe consume self.state {
    case .idle(var idle):
      unsafe self = .init(state: .terminating(.init(
        buffer: idle.buffer,
        failure: nil,
      )))
      return unsafe .call(
        terminationHandler: idle.terminationHandler.take()
      )

    case .waiting(var waiting):
      unsafe self = .init(state: .terminating(.init(
        buffer: [],
        failure: nil
      )))
      return unsafe .callAndResume(.init(
        consumers: waiting.consumers,
        terminationHandler: waiting.terminationHandler.take(),
      ))

    case .draining(let draining):
      // Already final: later terminations are ignored (first finish wins)
      unsafe self = .init(state: .draining(draining))
      return unsafe .none

    case .terminating(let terminating):
      // Already terminating: nothing to do, the cancellation path carries no
      // failure of its own (a failure can only be set via `finish(throwing:)`)
      unsafe self = .init(state: .terminating(terminating))
      return unsafe .none

    case .terminated(let terminated):
      // Already final: later terminations are ignored (first finish wins)
      unsafe self = .init(state: .terminated(terminated))
      return unsafe .none
    }
  }

  /// Reads the failure recorded in the terminal state after the termination
  /// handler has run, and finalizes to `terminated`.
  mutating func takeTerminalFailure() -> Failure? {
    switch unsafe consume self.state {
    case .idle:
      preconditionFailure("takeTerminalFailure() called in a non-terminal state")

    case .waiting:
      preconditionFailure("takeTerminalFailure() called in a non-terminal state")

    case .draining:
      preconditionFailure("takeTerminalFailure() called in a non-terminal state")

    case .terminating(var terminating):
      let failure = terminating.takeFailure()
      unsafe self = .init(state: .terminated(.init(
        terminationHandler: terminating.terminationHandler.take()
      )))
      return failure

    case .terminated(var terminated):
      let failure = terminated.failure.take()
      unsafe self = .init(state: .terminated(terminated))
      return failure
    }
  }

  /// Finalizes the `terminating` state to `terminated` preserving any recorded failure.
  mutating func finalizeTermination() {
    switch unsafe consume self.state {
    case .idle:
      preconditionFailure("finalizeTermination() called in a non-terminal state")

    case .waiting:
      preconditionFailure("finalizeTermination() called in a non-terminal state")

    case .draining(let draining):
      // `finish` with buffered elements left us draining
      unsafe self = .init(state: .draining(draining))

    case .terminating(var terminating):
      if terminating.buffer.isEmpty {
        unsafe self = .init(state: .terminated(.init(
          failure: terminating.failure,
          terminationHandler: terminating.terminationHandler.take()
        )))
      } else {
        unsafe self = .init(state: .draining(.init(
          buffer: terminating.buffer,
          failure: terminating.failure,
          terminationHandler: terminating.terminationHandler.take()
        )))
      }

    case .terminated(let terminated):
      unsafe self = .init(state: .terminated(terminated))
    }
  }
}

extension _AsyncStreamStorage {
  func getOnTermination() -> StateMachine.TerminationHandler? {
    return withLock { state in
      return state.getOnTermination()
    }
  }

  func setOnTermination(_ newValue: StateMachine.TerminationHandler?) {
    // The handler we're replacing must be released after the lock is
    // dropped: an adopter may have composed a chain of handlers, and releasing
    // that chain can run arbitrary `deinit` code
    let previous = withLock { state in
      return state.setOnTermination(newValue)
    }
    withExtendedLifetime(previous) {}
  }

  func yield(_ value: consuming sending Element) -> Continuation.YieldResult {
    var disconnected = Disconnected(value)
    let action = withLock { state in
      return unsafe state.yield(disconnected.take())
    }

    switch unsafe action {
    case .resume(let consumer, let element, let yieldResult):
      unsafe consumer.resume(returning: .success(element))
      return yieldResult

    case .none(let yieldResult):
      return yieldResult
    }
  }

  func next(_ consumer: consuming StateMachine.Consumer) {
    let action = withLock { state in
      return unsafe state.next(consumer)
    }

    switch unsafe action {
    case .resume(let consumer, let element):
      unsafe consumer.resume(returning: .success(element))

    case .throw(let consumer, let failure):
      unsafe consumer.resume(returning: .failure(failure))

    case .suspend:
      return
    }
  }

  nonisolated(nonsending) func next() async throws(Failure) -> Element? {
    return try await withTaskCancellationHandler {
      return unsafe await withUnsafeContinuation { consumer in
        unsafe self.next(consumer)
      }
    } onCancel: {
      self.terminate(.cancelled)
    }.get()
  }

  func terminate(_ terminationReason: Continuation.Termination) {
    let action =
      switch terminationReason {
      case .finished(let withFailure):
        withLock { state in
          return unsafe state.terminate(withFailure)
        }

      case .cancelled:
        // Only "begin" terminating here, next we'll trigger the cancellation handler,
        // which must be allowed to `finish(throwing:)` to finalize the termination with an error.
        withLock { state in
         return unsafe state.beginTerminating()
        }
      }

    switch unsafe consume action {
    case .callAndResume(var callAndResume):
      unsafe callAndResume.terminationHandler?.invoke(terminationReason)

      let failure = withLock { state in
        // Reload the failure, in case the termination handler has set one using `finish(throwing:)`.
        return state.takeTerminalFailure()
      }

      if let failure {
        let consumer = unsafe callAndResume.consumers.removeFirst()
        unsafe consumer.resume(returning: .failure(failure))
      }

      while let consumer = unsafe callAndResume.consumers.popFirst() {
        unsafe consumer.resume(returning: .success(nil))
      }

    case .call(let terminationHandler):
      terminationHandler?.invoke(terminationReason)
      withLock { state in
        state.finalizeTermination()
      }

    case .none:
      return
    }
  }
}

extension _AsyncStreamStorage {
  @safe
  private func withLock<Value: ~Copyable>(
    _ body: (inout StateMachine) -> Value
  ) -> Value {
    unsafe _lock(self.lock)

    defer { unsafe _unlock(self.lock) }

    return body(&self.stateMachine)
  }
}

final class _AsyncStreamCriticalStorage<Contents>: @unchecked Sendable {
  var _value: Contents
  private init(_doNotCallMe: ()) {
    fatalError("_AsyncStreamCriticalStorage must be initialized by create")
  }

  private func lock() {
    let ptr =
      unsafe UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    unsafe _lock(ptr)
  }

  private func unlock() {
    let ptr =
      unsafe UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    unsafe _unlock(ptr)
  }

  var value: Contents {
    get {
      lock()
      let contents = _value
      unlock()
      return contents
    }

    set {
      lock()
      withExtendedLifetime(_value) {
        _value = newValue
        unlock()
      }
    }
  }

  static func create(_ initial: Contents) -> _AsyncStreamCriticalStorage {
    let minimumCapacity = _lockWordCount()
    let storage = unsafe Builtin.allocWithTailElems_1(
      _AsyncStreamCriticalStorage.self,
      minimumCapacity._builtinWordValue,
      UnsafeRawPointer.self
    )

    let state =
      unsafe UnsafeMutablePointer<Contents>(Builtin.addressof(&storage._value))
    unsafe state.initialize(to: initial)
    let ptr = unsafe UnsafeRawPointer(
      Builtin.projectTailElems(storage, UnsafeRawPointer.self))
    unsafe _lockInit(ptr)
    return storage
  }
}
#endif
