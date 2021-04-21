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

@_silgen_name("_swift_series_lock_size")
func _lockWordCount() -> Int

@_silgen_name("_swift_series_lock_init")
func _lockInit(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_series_lock_lock")
func _lock(_ ptr: UnsafeRawPointer)

@_silgen_name("_swift_series_lock_unlock")
func _unlock(_ ptr: UnsafeRawPointer)

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
fileprivate final class _SeriesBufferedStorage<Element, Failure: Error>: UnsafeSendable {
  enum Terminal {
    case finished
    case failed(Failure)
  }

  struct State {
    // continuation can be either of two types:
    // UnsafeContinuation<Element?, Error>
    // UnsafeContinuation<Element?, Never>
    var continuation: Builtin.RawUnsafeContinuation?
    var pending = [Element]()
    var terminal: Terminal?
    var onCancel: (@Sendable () -> Void)?
    let limit: Int

    init(limit: Int) {
      self.limit = limit
    }
  }
  // Stored as a singular structured assignment for initialization
  var state: State

  private init(_doNotCallMe: ()) {
    fatalError("Storage must be initialized by create")
  }

  deinit {
    state.onCancel?()
  }

  private func lock() {
    let ptr =
      UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    _lock(ptr)
  }

  private func unlock() {
    let ptr =
      UnsafeRawPointer(Builtin.projectTailElems(self, UnsafeRawPointer.self))
    _unlock(ptr)
  }

  var onCancel: (@Sendable () -> Void)? {
    get {
      lock()
      let handler = state.onCancel
      unlock()
      return handler
    }
    set {
      lock()
      withExtendedLifetime(state.onCancel) {
        state.onCancel = newValue
        unlock()
      }
    }
  }

  func cancel() {
    lock()
    // swap out the handler before we invoke it to prevent double cancel
    let handler = state.onCancel
    state.onCancel = nil
    unlock()
    
    handler?() // handler must be invoked before yielding nil for termination

    yield(nil)
  }

  private func enqueue(_ value: __owned Element?) {
    if let value = value {
      if state.terminal == nil {
        state.pending.append(value)
        if state.limit < .max && state.pending.count > state.limit + 1 {
          state.pending.remove(at: 0)
        }
      }
    } else {
      terminate()
    }
  }

  private func terminate(error: __owned Failure? = nil) {
    state.onCancel = nil
    if let failure = error {
      state.terminal = .failed(failure)
    } else {
      state.terminal = .finished
    }
  }

  func yield(_ value: __owned Element?) {
    lock()
    enqueue(value)

    if let raw = state.continuation {
      if state.pending.count > 0 {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
        withExtendedLifetime((state.onCancel, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        case .failed(let error):
          continuation.resume(throwing: error)
        }
      } else {
        unlock()
      }
    } else {
      if state.limit == 0 {
        state.pending.remove(at: 0)
      }
      unlock()
    }
  }

  func yield(_ value: __owned Element?) where Failure == Never {
    lock()
    enqueue(value)

    if let raw = state.continuation {
      if state.pending.count > 0 {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Never>.self)
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        state.continuation = nil
        let continuation =
          unsafeBitCast(raw, to: UnsafeContinuation<Element?, Never>.self)
        withExtendedLifetime((state.onCancel, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        case .failed(let error):
          continuation.resume(throwing: error)
        }
      } else {
        unlock()
      }
    } else {
      if state.limit == 0 {
        state.pending.remove(at: 0)
      }
      unlock()
    }
  }

  func yield(throwing error: __owned Failure) {
    lock()
    if let raw = state.continuation {
      state.continuation = nil
      let continuation =
        unsafeBitCast(raw, to: UnsafeContinuation<Element?, Error>.self)
      withExtendedLifetime((state.onCancel, state.terminal)) {
        terminate()
        unlock()
      }
      continuation.resume(throwing: error)
    } else {
      withExtendedLifetime((state.onCancel, state.terminal)) {
        terminate(error: error)
        unlock()
      }
    }
  }

  func next(_ continuation: UnsafeContinuation<Element?, Never>) {
    let raw =
      unsafeBitCast(continuation, to: Builtin.RawUnsafeContinuation.self)

    lock()
    if state.continuation == nil {
      if state.pending.count > 0 {
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        withExtendedLifetime((state.onCancel, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        default:
          fatalError("Internal inconsistency, expecting non throwing continuation but found an error")
        }
      } else {
        state.continuation = raw
        unlock()
      }
    } else {
      unlock()
      fatalError("attempt to await next() on more than one task")
    }
  }

  func next(_ continuation: UnsafeContinuation<Element?, Error>) {
    let raw =
      unsafeBitCast(continuation, to: Builtin.RawUnsafeContinuation.self)

    lock()
    if state.continuation == nil {
      if state.pending.count > 0 {
        let toSend = state.pending.first
        state.pending.remove(at: 0)
        unlock()
        continuation.resume(returning: toSend)
      } else if let terminal = state.terminal {
        withExtendedLifetime((state.onCancel, state.terminal)) {
          terminate()
          unlock()
        }
        switch terminal {
        case .finished:
          continuation.resume(returning: nil)
        case .failed(let error):
          continuation.resume(throwing: error)
        }
      } else {
        state.continuation = raw
        unlock()
      }
    } else {
      unlock()
      fatalError("attempt to await next() on more than one task")
    }
  }

  static func create(limit: Int) -> _SeriesBufferedStorage<Element, Failure> {
    let minimumCapacity = _lockWordCount()
    let storage = Builtin.allocWithTailElems_1(
        _SeriesBufferedStorage<Element, Failure>.self,
        minimumCapacity._builtinWordValue,
        UnsafeRawPointer.self
    )

    let state =
      UnsafeMutablePointer<State>(Builtin.addressof(&storage.state))
    state.initialize(to: State(limit: limit))
    let ptr = UnsafeRawPointer(
      Builtin.projectTailElems(storage, UnsafeRawPointer.self))
    _lockInit(ptr)
    return storage
  }
}

/// An ordered, asynchronously generated sequence of elements.
///
/// Series is an interface type to adapt from code producing values to an
/// asynchronous context iterating them. This is itended to be used to allow
/// callback or delegation based APIs to participate with async/await.
///
/// When values are produced from a non async/await source there is a
/// consideration that must be made on behavioral characteristics of how that
/// production of values interacts with the iteration. Series offers a
/// initialization strategy that provides a method of yielding values into
/// iteration.
///
/// Series can be initialized with the option to buffer to a given limit.
/// The default value for this limit is Int.max. The buffering is only for
/// values that have yet to be consumed by iteration. Values can be yielded in
/// case to the continuation passed into the build closure. That continuation
/// is Sendable, in that it is intended to be used from concurrent contexts
/// external to the iteration of the Series.
///
/// A trivial use case producing values from a detached task would work as such:
///
///     let digits = Series(buffering: Int.self) { continuation in
///       detach {
///         for digit in 0..<10 {
///           continuation.resume(yielding: digit)
///         }
///         continuation.finish()
///       }
///     }
///
///     for await digit in digits {
///       print(digit)
///     }
///
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct Series<Element> {
  public struct Continuation: Sendable {
    fileprivate let storage: _SeriesBufferedStorage<Element, Never>

    /// Resume the task awaiting the next iteration point by having it return
    /// nomally from its suspension point or buffer the value if no awaiting
    /// next iteration is active.
    ///
    /// - Parameter value: The value to yield from the continuation.
    ///
    /// This can be called more than once and returns to the caller immediately
    /// without blocking for any awaiting consuption from the iteration.
    public func resume(yielding value: __owned Element) {
      storage.yield(value)
    }

    /// Resume the task awaiting the next iteration point by having it return
    /// nil which signifies the end of the iteration.
    ///
    /// Calling this function more than once is idempotent; i.e. finishing more
    /// than once does not alter the state beyond the requirements of
    /// AsyncSequence; which claims that all values past a terminal state are
    /// nil.
    public func finish() {
      storage.yield(nil)
    }

    /// A callback to invoke when iteration of a Series is cancelled.
    ///
    /// If an `onCancel` callback is set, when iteration of a Series is
    /// cancelled via task cancellation that callback is invoked. The callback
    /// is disposed of after any terminal state is reached.
    ///
    /// Cancelling an active iteration will first invoke the onCancel callback
    /// and then resume yeilding nil. This means that any cleanup state can be
    /// emitted accordingly in the cancellation handler
    public var onCancel: (@Sendable () -> Void)? {
      get {
        return storage.onCancel
      }
      nonmutating set {
        storage.onCancel = newValue
      }
    }
  }

  let produce: (UnsafeContinuation<Element?, Never>) -> Void
  let cancel: @Sendable () -> Void

  /// Construct a Series buffering given an Element type.
  ///
  /// - Parameter buffering: The type the Series will produce.
  /// - Parameter maxBufferedPendingElements: The maximum number of elements to
  ///   hold in the buffer past any checks for continuations being resumed.
  /// - Parameter build: The work associated with yielding values to the Series.
  ///
  /// The maximum number of pending elements limited by dropping the oldest
  /// value when a new value comes in if the buffer would excede the limit
  /// placed upon it. By default this limit is unlimited.
  ///
  /// The build closure passes in a Continuation which can be used in
  /// concurrent contexts. It is thread safe to send and finish; all calls are
  /// to the continuation are serialized, however calling this from multiple
  /// concurrent contexts could result in out of order delivery.
  public init(
    buffering: Element.Type = Element.self,
    maxBufferedPendingElements limit: Int = .max,
    _ build: (Continuation) -> Void
  ) {
    let storage: _SeriesBufferedStorage<Element, Never> = .create(limit: limit)
    produce = storage.next
    cancel = {
      storage.cancel()
    }
    build(Continuation(storage: storage))
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Series: AsyncSequence {
  /// The asynchronous iterator for iterating a Series.
  ///
  /// This type is specificially not Sendable. It is not intended to be used
  /// from multiple concurrent contexts. Any such case that next is invoked
  /// concurrently and contends with another call to next is a programmer error
  /// and will fatalError.
  public struct Iterator: AsyncIteratorProtocol {
    let produce: (UnsafeContinuation<Element?, Never>) -> Void
    let cancel: @Sendable () -> Void

    /// The next value from the Series.
    ///
    /// When next returns nil this signifies the end of the Series. Any such
    /// case that next is invoked concurrently and contends with another call to
    /// next is a programmer error and will fatalError.
    ///
    /// If the task this iterator is running in is canceled while next is
    /// awaiting a value, this will terminate the Series and next may return nil
    /// immediately (or will return nil on subseuqent calls)
    public mutating func next() async -> Element? {
      return await withTaskCancellationHandler { [cancel] in
        cancel()
      } operation: {
        return await withUnsafeContinuation {
          (continuation: UnsafeContinuation<Element?, Never>) in
          produce(continuation)
        }
      }
    }
  }

  /// Construct an iterator.
  public func makeAsyncIterator() -> Iterator {
    return Iterator(produce: produce, cancel: cancel)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Series.Continuation {
  /// Resume the task awaiting the next iteration point by having it return
  /// nomally from its suspension point or buffer the value if no awaiting
  /// next iteration is active.
  ///
  /// - Parameter result: A result to yield from the continuation.
  ///
  /// This can be called more than once and returns to the caller immediately
  /// without blocking for any awaiting consuption from the iteration.
  public func yield(
    with result: Result<Element, Never>
  ) {
    switch result {
      case .success(let val):
        self.resume(yielding: val)
    }
  }

  /// Resume the task awaiting the next iteration point by having it return
  /// nomally from its suspension point or buffer the value if no awaiting
  /// next iteration is active where the `Element` is `Void`.
  ///
  /// This can be called more than once and returns to the caller immediately
  /// without blocking for any awaiting consuption from the iteration.
  public func resume() where Element == Void {
    self.resume(yielding: ())
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct ThrowingSeries<Element> {
  public struct Continuation: Sendable {
    fileprivate let storage: _SeriesBufferedStorage<Element, Error>

    /// Resume the task awaiting the next iteration point by having it return
    /// nomally from its suspension point or buffer the value if no awaiting
    /// next iteration is active.
    ///
    /// - Parameter value: The value to yield from the continuation.
    ///
    /// This can be called more than once and returns to the caller immediately
    /// without blocking for any awaiting consuption from the iteration.
    public func resume(yielding value: __owned Element) {
      storage.yield(value)
    }

    /// Resume the task awaiting the next iteration point by having it return
    /// nil or throw which signifies the end of the iteration.
    ///
    /// - Parameter error: The error to throw or nil to signify termination.
    ///
    /// Calling this function more than once is idempotent; i.e. finishing more
    /// than once does not alter the state beyond the requirements of
    /// AsyncSequence; which claims that all values past a terminal state are
    /// nil.
    public func finish(throwing error: __owned Error? = nil) {
      if let failure = error {
        storage.yield(throwing: failure)
      } else {
        storage.yield(nil)
      }
    }

    /// A callback to invoke when iteration of a ThrowingSeries is cancelled.
    ///
    /// If an `onCancel` callback is set, when iteration of a Series is
    /// cancelled via task cancellation that callback is invoked. The callback
    /// is disposed of after any terminal state is reached.
    ///
    /// Cancelling an active iteration will first invoke the onCancel callback
    /// and then resume yeilding nil. This means that any cleanup state can be
    /// emitted accordingly in the cancellation handler
    public var onCancel: (@Sendable () -> Void)? {
      get {
        return storage.onCancel
      }
      nonmutating set {
        storage.onCancel = newValue
      }
    }
  }

  let produce: (UnsafeContinuation<Element?, Error>) -> Void
  let cancel: @Sendable () -> Void

  /// Construct a ThrowingSeries buffering given an Element type.
  ///
  /// - Parameter buffering: The type the Series will produce.
  /// - Parameter maxBufferedPendingElements: The maximum number of elements to
  ///   hold in the buffer past any checks for continuations being resumed.
  /// - Parameter build: The work associated with yielding values to the Series.
  ///
  /// The maximum number of pending elements limited by dropping the oldest
  /// value when a new value comes in if the buffer would excede the limit
  /// placed upon it. By default this limit is unlimited.
  ///
  /// The build closure passes in a Continuation which can be used in
  /// concurrent contexts. It is thread safe to send and finish; all calls are
  /// to the continuation are serialized, however calling this from multiple
  /// concurrent contexts could result in out of order delivery.
  public init(
    buffering: Element.Type,
    maxBufferedPendingElements limit: Int = .max,
    _ build: (Continuation) -> Void
  ) {
    let storage: _SeriesBufferedStorage<Element, Error> = .create(limit: limit)
    produce = storage.next
    cancel = {
      storage.cancel()
    }
    build(Continuation(storage: storage))
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension ThrowingSeries: AsyncSequence {
  /// The asynchronous iterator for iterating a ThrowingSeries.
  ///
  /// This type is specificially not Sendable. It is not intended to be used
  /// from multiple concurrent contexts. Any such case that next is invoked
  /// concurrently and contends with another call to next is a programmer error
  /// and will fatalError.
  public struct Iterator: AsyncIteratorProtocol {
    let produce: (UnsafeContinuation<Element?, Error>) -> Void
    let cancel: @Sendable () -> Void

    public mutating func next() async throws -> Element? {
      return try await withTaskCancellationHandler { [cancel] in
        cancel()
      } operation: {
        return try await withUnsafeThrowingContinuation {
          (continuation: UnsafeContinuation<Element?, Error>) in
          produce(continuation)
        }
      }
    }
  }

  /// Construct an iterator.
  public func makeAsyncIterator() -> Iterator {
    return Iterator(produce: produce, cancel: cancel)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension ThrowingSeries.Continuation {
  /// Resume the task awaiting the next iteration point by having it return
  /// nomally from its suspension point or buffer the value if no awaiting
  /// next iteration is active.
  ///
  /// - Parameter result: A result to yield from the continuation.
  ///
  /// This can be called more than once and returns to the caller immediately
  /// without blocking for any awaiting consuption from the iteration.
  public func resume<Failure: Error>(
    with result: Result<Element, Failure>
  ) {
    switch result {
      case .success(let val):
        self.resume(yielding: val)
      case .failure(let err):
        self.finish(throwing: err)
    }
  }

  /// Resume the task awaiting the next iteration point by having it return
  /// nomally from its suspension point or buffer the value if no awaiting
  /// next iteration is active where the `Element` is `Void`.
  ///
  /// This can be called more than once and returns to the caller immediately
  /// without blocking for any awaiting consuption from the iteration.
  public func resume() where Element == Void {
    self.resume(yielding: ())
  }
}


