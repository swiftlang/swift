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

fileprivate
struct _UnsafeSendable<Value: ~Copyable>: @unchecked Sendable, ~Copyable {
  private let value: Value

  init(_ value: consuming Value) {
    self.value = value
  }

  consuming func take() -> sending Value {
    return self.value
  }
}

@safe
internal
final class _Storage<Element, Failure: Error>: @unchecked Sendable {
  typealias Buffer = _Deque<Element>
  typealias Consumer = UnsafeContinuation<Result<Element?, Failure>, Never>
  typealias Consumers = _Deque<UnsafeContinuation<Result<Element?, Failure>, Never>>
  typealias TerminationHandler = @Sendable (Continuation.Termination) -> Void

  @unsafe
  private
  enum State {
    case idle(
      buffer: Buffer)

    case waiting(
      consumers: Consumers)

    case draining(
      buffer: Buffer,
      failure: Failure? = nil)

    case terminated(
      failure: Failure? = nil)
  }

  @unsafe
  private
  enum YieldAction {
    case resume(
      consumer: Consumer,
      element: Element?)

    case none
  }

  private
  enum NextAction {
    case resume(
      element: Element?)

    case `throw`(
      failure: Failure)

    case suspend
  }

  @unsafe
  private
  enum TerminateAction {
    case callHandlerAndResume(
      terminationHandler: TerminationHandler?,
      consumers: Consumers,
      failure: Failure?)

    case callHandler(
      terminationHandler: TerminationHandler?)

    case none
  }

  private var state: State
  private var bufferPolicy: Continuation.BufferingPolicy
  private var onTermination: TerminationHandler?

  private init(_doNotCallMe: ()) {
    fatalError("Storage must be initialized by create")
  }

  deinit {
    self.terminate(.cancelled)
  }
}

extension _Storage {
  func getOnTermination() -> TerminationHandler? {
    unsafe withLock {
      return self.onTermination
    }
  }

  func setOnTermination(_ newValue: TerminationHandler?) {
    unsafe withLock {
      switch unsafe self.state {
      case .idle, .waiting:
        self.onTermination = newValue

      case .draining, .terminated:
        return
      }
    }
  }

  func yield(_ value: sending Element) -> Continuation.YieldResult {
    let (
      result,
      action
    ): (Continuation.YieldResult, YieldAction) = unsafe withLock {
      switch unsafe self.state {
      case var .idle(buffer):
        switch self.bufferPolicy {
        case .unbounded:
          buffer.append(value)
          unsafe self.state = unsafe .idle(buffer: buffer)
          return unsafe (
            result: .enqueued(remaining: .max),
            action: .none)

        case let .bufferingOldest(limit):
          switch buffer.count < limit {
          case true:
            buffer.append(value)
            unsafe self.state = unsafe .idle(buffer: buffer)
            return unsafe (
              result: .enqueued(remaining: limit - buffer.count),
              action: .none)

          case false:
            return unsafe (
              result: .dropped(value),
              action: .none)
          }

        case let .bufferingNewest(limit):
          switch limit {
          case let limit where limit <= .zero:
            return unsafe (
              result: .dropped(value),
              action: .none)

          case let limit where buffer.count < limit:
            buffer.append(value)
            unsafe self.state = unsafe .idle(buffer: buffer)
            return unsafe (
              result: .enqueued(remaining: limit - buffer.count),
              action: .none)

          default:
            let droppedValue = buffer.removeFirst()
            buffer.append(value)
            unsafe self.state = unsafe .idle(buffer: buffer)
            return unsafe (
              result: .dropped(droppedValue),
              action: .none)
          }
        }

      case var .waiting(consumers):
        let consumer = unsafe consumers.removeFirst()

        switch unsafe consumers.isEmpty {
        case true:
          unsafe self.state = unsafe .idle(buffer: [])
        case false:
          unsafe self.state = unsafe .waiting(consumers: consumers)
        }

        switch self.bufferPolicy {
        case .unbounded:
          return unsafe (
            result: .enqueued(remaining: .max),
            action: .resume(consumer: consumer, element: value))

        case let .bufferingOldest(limit), let .bufferingNewest(limit):
          return unsafe (
            result: .enqueued(remaining: limit),
            action: .resume(consumer: consumer, element: value))
        }

      case .draining, .terminated:
        return unsafe (
          result: .terminated,
          action: .none)
      }
    }

    switch unsafe action {
    case let .resume(consumer, element):
      let element = _UnsafeSendable(element).take()
      unsafe consumer.resume(returning: .success(element))
      return result

    case .none:
      return result
    }
  }

  private
  func next(_ consumer: Consumer) {
    let action: NextAction = unsafe withLock {
      switch unsafe self.state {
      case var .idle(buffer):
        switch buffer.isEmpty {
        case true:
          unsafe self.state = unsafe .waiting(consumers: [consumer])
          return .suspend

        case false:
          let element = buffer.removeFirst()
          unsafe self.state = unsafe .idle(buffer: buffer)
          return .resume(element: element)
        }

      case var .waiting(consumers):
        unsafe consumers.append(consumer)
        unsafe self.state = .waiting(consumers: consumers)
        return .suspend

      case .draining(var buffer, let failure):
        switch buffer.isEmpty {
        case true:
          unsafe self.state = unsafe .terminated()
          switch failure {
          case .none:
            return .resume(element: nil)

          case let .some(failure):
            return .throw(failure: failure)
          }

        case false:
          let element = buffer.removeFirst()
          unsafe self.state = unsafe .draining(buffer: buffer, failure: failure)
          return .resume(element: element)
        }

      case let .terminated(failure):
        unsafe self.state = unsafe .terminated()
        switch failure {
        case .none:
          return .resume(element: nil)

        case let .some(failure):
          return .throw(failure: failure)
        }
      }
    }

    switch action {
    case let .resume(element):
      unsafe consumer.resume(returning: .success(element))

    case let .throw(failure):
      unsafe consumer.resume(returning: .failure(failure))

    case .suspend:
      break
    }
  }

  nonisolated(nonsending)
  func next() async throws(Failure) -> Element? {
    return try await withTaskCancellationHandler {
      return unsafe await withUnsafeContinuation { consumer in
        unsafe self.next(consumer)
      }
    } onCancel: {
      self.terminate(.cancelled)
    }.get()
  }

  func terminate(_ terminationReason: Continuation.Termination) {
    let action: TerminateAction = unsafe withLock {
      let failure: Failure?

      switch terminationReason {
      case let .finished(withFailure):
        failure = withFailure

      case .cancelled:
        failure = nil
      }

      switch unsafe self.state {
      case let .idle(buffer):
        switch buffer.isEmpty {
        case true:
          unsafe self.state = unsafe .terminated(failure: failure)

        case false:
          unsafe self.state = unsafe .draining(buffer: buffer, failure: failure)
        }
        return unsafe .callHandler(
          terminationHandler: self.onTermination.take())

      case let .waiting(consumers):
        unsafe self.state = .terminated()
        return unsafe .callHandlerAndResume(
          terminationHandler: self.onTermination.take(),
          consumers: consumers,
          failure: failure)

      case .draining, .terminated:
        return unsafe .none
      }
    }

    switch unsafe action {
    case .callHandlerAndResume(
      terminationHandler: let terminationHandler,
      consumers: var consumers,
      failure: let failure):
      terminationHandler?(terminationReason)

      if let failure {
        let consumer = unsafe consumers.popFirst()
        unsafe consumer?.resume(returning: .failure(failure))
      }

      while let element = unsafe consumers.popFirst() {
        unsafe element.resume(returning: .success(nil))
      }

    case let .callHandler(terminationHandler: terminationHandler):
      terminationHandler?(terminationReason)

    case .none:
      break
    }
  }
}

extension _Storage {
  internal
  struct Continuation {
    internal
    enum BufferingPolicy {
      case unbounded

      case bufferingOldest(Int)

      case bufferingNewest(Int)
    }

    internal
    enum YieldResult {
      case enqueued(remaining: Int)

      case dropped(Element)

      case terminated
    }

    internal
    enum Termination {
      case finished(Failure?)

      case cancelled
    }
  }
}

extension _Storage {
  private
  func withLock<T, E: Error>(
    _ action: () throws(E) -> T
  ) throws(E) -> T {
    let ptr =
    unsafe UnsafeRawPointer(
      Builtin.projectTailElems(self, UnsafeRawPointer.self)
    )

    unsafe _lock(ptr)

    defer { unsafe _unlock(ptr) }

    return try action()
  }

  static func create(
    bufferPolicy limit: Continuation.BufferingPolicy
  ) -> _Storage {
    let minimumCapacity = _lockWordCount()

    let storage = unsafe Builtin.allocWithTailElems_1(
      _Storage.self,
      minimumCapacity._builtinWordValue,
      UnsafeRawPointer.self
    )

    let bufferPolicyPtr =
    unsafe UnsafeMutablePointer<Continuation.BufferingPolicy>(
      Builtin.addressof(&storage.bufferPolicy)
    )
    unsafe bufferPolicyPtr.initialize(to: limit)

    let statePtr =
    unsafe UnsafeMutablePointer<State>(
      Builtin.addressof(&storage.state)
    )
    unsafe statePtr.initialize(to: .idle(buffer: []))

    let terminationPtr =
    unsafe UnsafeMutablePointer<TerminationHandler?>(
      Builtin.addressof(&storage.onTermination)
    )
    unsafe terminationPtr.initialize(to: nil)

    let ptr =
    unsafe UnsafeRawPointer(
      Builtin.projectTailElems(storage, UnsafeRawPointer.self)
    )
    unsafe _lockInit(ptr)

    return storage
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
