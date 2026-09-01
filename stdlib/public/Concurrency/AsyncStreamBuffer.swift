//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2026 Apple Inc. and the Swift project authors
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
///   - `draining`: The stream accepts **no new elements**,
///   consumers drain the buffer, which **initially contains at least one element**.
///   - `terminated`: The stream is in a terminal state,
///   **no new elements are accepted**, and **new consumers return immediately**.
///
/// Transitions:
///
/// ```text
/// Current State   Possible Next State
/// -------------   -------------------
/// idle          ->  idle, waiting, draining, terminated
/// waiting       ->  idle, waiting, terminated
/// draining      ->  draining, terminated
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
/// Finishing Behavior:
/// A throwing stream that has been **terminated due to cancellation is unfinished**,
/// and can be **finished once** with a specific error
/// by calling the `finish(throwing:)` method.
///
/// Specifically, an unfinished terminal stream is itself a transient state
/// during the termination process. If no call to `finish()` / `finish(throwing:)`
/// occurs during this period, e.g., from within the `onTermination` closure,
/// the stream will be **automatically finished** after the `onTermination`
/// closure has been invoked. Formally, this automatic finishing behavior
/// happens after the `onTermination` closure has been invoked.
///
/// - Note: While the same mechanism applies to `AsyncStream`,
/// since it is non-throwing, whether the termination reason is cancellation
/// or the stream being finished makes no difference: a terminal `AsyncStream`
/// that has drained all its buffered elements will always return `nil`.
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
    typealias TerminationReason = Continuation.Termination
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
        var terminationReason: TerminationReason
        var terminationHandler: TerminationHandler?
      }

      struct Terminated: ~Copyable {
        var terminationReason: TerminationReason
        var terminationHandler: TerminationHandler? // TODO: Remove this in a follow-up PR
      }

      case idle(Idle)

      case waiting(Waiting)

      case draining(Draining)

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
      let element = draining.buffer.removeFirst()

      if draining.buffer.isEmpty {
        unsafe self = .init(state: .terminated(.init(
          terminationReason: draining.terminationReason,
          terminationHandler: draining.terminationHandler.take()
        )))

      } else {
        unsafe self = .init(state: .draining(draining))
      }

      return unsafe .resume(
        consumer: consumer,
        element: element
      )

    case .terminated(let terminated):
      switch terminated.terminationReason {
      case .finished(let failure?):
        unsafe self = .init(state: .terminated(.init(
          terminationReason: .finished(nil)
        )))
        return unsafe .throw(
          consumer: consumer,
          failure: failure
        )

      default:
        unsafe self = .init(state: .terminated(.init(
          terminationReason: terminated.terminationReason
        )))
        return unsafe .resume(
          consumer: consumer,
          element: nil
        )
      }
    }
  }

  mutating func terminate(_ terminationReason: TerminationReason) -> TerminateAction {
    switch unsafe consume self.state {
    case .idle(var idle):
      if idle.buffer.isEmpty {
        unsafe self = .init(state: .terminated(.init(
          terminationReason: terminationReason
        )))

      } else {
        unsafe self = .init(state: .draining(.init(
          buffer: idle.buffer,
          terminationReason: terminationReason
        )))
      }

      return unsafe .call(
        terminationHandler: idle.terminationHandler.take()
      )

    case .waiting(var waiting):
      unsafe self = .init(state: .terminated(.init(
        terminationReason: terminationReason
      )))
      return unsafe .callAndResume(.init(
        consumers: waiting.consumers,
        terminationHandler: waiting.terminationHandler.take(),
      ))

    case .draining(var draining):
      if case .cancelled = draining.terminationReason {
        draining.terminationReason = terminationReason
      }
      unsafe self = .init(state: .draining(draining))
      return unsafe .none

    case .terminated(var terminated):
      if case .cancelled = terminated.terminationReason {
        terminated.terminationReason = terminationReason
      }
      unsafe self = .init(state: .terminated(terminated))
      return unsafe .none
    }
  }

  // called only from `terminate(_:)` when `action` is `.callAndResume`.
  mutating func finalizeFromCallAndResumeAction() -> Failure? {
    switch unsafe consume self.state {
    case .idle:
      fatalError("Unexpected state; called in a non-terminal state")

    case .waiting:
      fatalError("Unexpected state; called in a non-terminal state")

    case .draining:
      fatalError("Unexpected state; called in the wrong state")

    case .terminated(var terminated):
      let terminationReason = terminated.terminationReason
      terminated.terminationReason = .finished(nil)
      unsafe self = .init(state: .terminated(terminated))

      switch terminationReason {
      case .finished(let failure):
        return failure

      case .cancelled:
        return nil
      }
    }
  }

  // called only from `terminate(_:)` when `action` is `.call`.
  mutating func finalizeFromCallAction() {
    switch unsafe consume self.state {
    case .idle:
      fatalError("Unexpected state; called in a non-terminal state")

    case .waiting:
      fatalError("Unexpected state; called in a non-terminal state")

    case .draining(var draining):
      if case .cancelled = draining.terminationReason {
        draining.terminationReason = .finished(nil)
      }
      unsafe self = .init(state: .draining(draining))

    case .terminated(var terminated):
      if case .cancelled = terminated.terminationReason {
        terminated.terminationReason = .finished(nil)
      }
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
    let action = withLock { state in
      return unsafe state.terminate(terminationReason)
    }

    switch unsafe consume action {
    case .callAndResume(var callAndResume):
      unsafe callAndResume.terminationHandler?.invoke(terminationReason)

      let failure = withLock { state in
        return state.finalizeFromCallAndResumeAction()
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
        state.finalizeFromCallAction()
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
