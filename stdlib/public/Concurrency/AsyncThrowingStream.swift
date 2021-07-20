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

/// An asynchronous sequence generated from an error-throwing closure that
/// calls a continuation to produce new elements.
///
/// `AsyncThrowingStream` conforms to `AsyncSequence`, providing a convenient
/// way to create an asynchronous sequence without manually implementing an
/// asynchronous iterator. In particular, an asynchronous stream is well-suited
/// to adapt callback- or delegation-based APIs to participate with
/// `async`/`await`.
///
/// In contrast to `AsyncStream`, this type can throw an error from the awaited
/// `next()`, which terminates the stream with the thrown error.
///
/// You initialize an `AsyncThrowingStream` with a closure that receives an
/// `AsyncThrowingStream.Continuation`. Produce elements in this closure, then
/// provide them to the stream by calling the continuation's `yield(_:)` method.
/// When there are no further elements to produce, call the continuation's
/// `finish()` method. This causes the sequence iterator to produce a `nil`,
/// which terminates the sequence. If an error occurs, call the continuation's
/// `finish(throwing:)` method, which causes the iterator's `next()` to throw
/// the error to the awaiting call point. The continuation is `Sendable`, which
/// permits calling it from concurrent contexts external to
/// the iteration of the `AsyncThrowingStream`.
///
/// An arbitrary source of elements may produce elements faster than they are
/// consumed by a caller iterating over them. Because of this, `AsyncThrowingStream`
/// defines a buffering behavior, allowing the stream to buffer a specific
/// number of oldest or newest elements. By default, the buffer limit is
/// unbounded, that is, `Int.max`.
///
/// ### Adapting Existing Code to Use Streams
///
/// To adapt existing callback code to use `async` / `await`, use the callbacks
/// to provide values to the stream, by using the continuation's `yield(_:)`
/// method.
///
/// Consider a hypothetical `QuakeMonitor` type that provides callers with
/// `Quake` instances every time it detects an earthquake. To receive callbacks,
/// callers set a custom closure a closure as the value of the monitor's
/// `quakeHandler` property, which the monitor calls back as necessary. Callers
/// can also set an `errorHandler` to receive asychronous error notifications,
/// such as the monitor service suddenly becoming unavailable.
///
///     class QuakeMonitor {
///         var quakeHandler: ((Quake) -> Void)?
///         var errorHandler: ((Error) -> Void)?
///
///         func startMonitoring() {…}
///         func stopMonitoring() {…}
///     }
///
/// To adapt this to use `async`-`await`, extend the `QuakeMonitor` to add a
/// `quakes` property, of type `AsyncStream<Quake>`. In the getter for this
/// property, return an `AsyncStream`, whose `build` closure -- called at
/// runtime to create the stream -- does the following:
///
/// 1. Creates a `QuakeMonitor` instance.
/// 2. Sets the monitor's `quakeHandler` property to a closure that receives
/// each `Quake` instance and forwards it to the stream by calling the
/// continuation's `yield(_:)` method.
/// 3. Sets the monitor's `errorHandler` property to a closure that receives
/// any error from the monitor and forwards it to the stream by calling the
/// continuation's `finish(throwing:)` method. This causes the stream's
/// iterator to throw the error, terminating the stream.
/// 4. Sets the continuation's `onTermination` property to a closure that
/// calls `stopMonitoring()` on the monitor.
/// 5. Finally, calls `startMonitoring` on the `QuakeMonitor`.
///
///     extension QuakeMonitor {
///
///         static var throwingQuakes: AsyncThrowingStream<Quake, Error> {
///             AsyncThrowingStream { continuation in
///                 let monitor = QuakeMonitor()
///                 monitor.quakeHandler = { quake in
///                     continuation.yield(quake)
///                 }
///                 monitor.errorHandler = { error in
///                     continuation.finish(throwing: error)
///                 }
///                 continuation.onTermination = { @Sendable _ in
///                     monitor.stopMonitoring()
///                 }
///                 monitor.startMonitoring()
///             }
///         }
///     }
///
///
/// Since the stream is an `AsyncSequence`, the call point can use the
/// `for`-`await`-`in` syntax to process each `Quake` instance as the stream
/// produces it:
///
///     do {
///         for try await quake in quakeStream {
///             print ("Quake: \(quake.date)")
///         }
///         print ("Stream done.")
///     } catch {
///         print ("Error: \(error)")
///     }
///
@available(SwiftStdlib 5.5, *)
public struct AsyncThrowingStream<Element, Failure: Error> {

    /// A mechanism to interface between synchronous code and an asynchronous
    /// stream.
    ///
    /// The closure you provide to the `AsyncThrowingStream` in
    /// `init(_:bufferingPolicy:_:)` receives an instance of this type when
    /// invoked. Use this continuation to provide elements to the stream by
    /// calling one of the `yield` methods, then terminate the stream normally by
    /// calling the `finish()` method. You can also use the continuation's
    /// `finish(throwing:)` method to terminate the stream by throwing an error.
    public struct Continuation: Sendable {
    /// A type that indicates how the stream terminated.
    ///
    /// The `onTermination` closure receives an instance of this type.
    public enum Termination {
      
     /// The stream finished as a result of calling the continuation's
     ///  `finish` method.
     ///
     ///  The associated `Failure` value provides the error that terminated
     ///  the stream. If no error occurred, this value is `nil`.
      case finished(Failure?)
      
     /// The stream finished as a result of cancellation.
      case cancelled
    }
    
    /// A type that indicates the result of yielding a value to a client, by
    /// way of the continuation.
    public enum YieldResult {
      
      /// The stream successfully enqueued the element.
      ///
      /// This value reprsents the successful enqueueing of an element, whether
      /// the stream buffers the element or delivers it immediately to a pending
      /// call to `next()`. The associated value `remaining` indicates the
      /// number of remaining slots in the buffer at the point in time of
      /// yielding.
      ///
      /// > Note: Acting on the remaining count is valid only when calls to
      /// yield are mutually exclusive.
      case enqueued(remaining: Int)
      
      /// The stream did not enqueue the element due to a full buffer.
      ///
      /// The associated element for this case is the element that the stream
      /// dropped.
      case dropped(Element)
      
     /// The stream did not enqueue the element because the stream was in a
     /// terminal state.
     ///
     /// This indicates the stream terminated prior to calling `yield`, either
     /// because the stream finished normally or through cancellation.
      case terminated
    }
    
    /// A strategy that handles exhaustion of a buffer’s capacity.
    public enum BufferingPolicy {

      /// Continue to add to the buffer, treating its capacity as infinite.
      case unbounded
      
      /// When the buffer is full, discard the newly received element.
      ///
      /// This strategy enforces keeping the specified amount of oldest values.
      case bufferingOldest(Int)
      
      /// When the buffer is full, discard the oldest element in the buffer.
      ///
      /// This strategy enforces keeping the specified amount of newest values.
      case bufferingNewest(Int)
    }

    let storage: _Storage

    /// Resume the task awaiting the next iteration point by having it return
    /// nomally from its suspension point with a given element.
    ///
    /// - Parameter value: The value to yield from the continuation.
    /// - Returns: A `YieldResult` indicating the success or failure of the
    ///   yield operation.
    ///
    /// If nothing is awaiting the next value, this call attempts to buffer the
    /// result's element.
    ///
    /// This can be called more than once and returns to the caller immediately
    /// without blocking for any awaiting consuption from the iteration.
    @discardableResult
    public func yield(_ value: __owned Element) -> YieldResult {
      storage.yield(value)
    }

    /// Resume the task awaiting the next iteration point by having it return
    /// nil, which signifies the end of the iteration.
    ///
    /// - Parameter error: The error to throw, or `nil` to finish normally.
    ///
    /// Calling this function more than once has no effect. Once you call
    /// finish, the stream enters a terminal state and produces no further
    /// elements.
    public func finish(throwing error: __owned Failure? = nil) {
      storage.finish(throwing: error)
    }

    /// A callback to invoke when canceling iteration of an asynchronous
    /// stream.
    ///
    /// If an `onTermination` callback is set, using task cancellation to
    /// terminate iteration of a `AsyncThrowingStream` results in a call to this
    /// callback.
    ///
    /// Cancelling an active iteration invokes the `onTermination` callback
    /// first, then resumes by yielding `nil` or throwing an error from the
    /// iterator. This means that you can perform needed cleanup in the
    ///  cancellation handler. After reaching a terminal state, the
    ///   `AsyncThrowingStream` disposes of the callback.
    public var onTermination: (@Sendable (Termination) -> Void)? {
      get {
        return storage.getOnTermination()
      }
      nonmutating set {
        storage.setOnTermination(newValue)
      }
    }
  }

  let produce: () async throws -> Element?

  /// Constructs an asynchronous stream for an element type, using the
  /// specified buffering policy and element-producing closure.
  ///
  /// - Parameter elementType: The type of element the `AsyncStream`
  ///   produces
  /// - Parameter maxBufferedElements: The maximum number of elements to
  ///   hold in the buffer. By default, this value is unlimited. Use a
  ///   `Continuation.BufferingPolicy` to buffer a specified number of oldest
  ///   or newest elements.
  /// - Parameter build: A custom closure that yields values to the
  ///   `AsyncStream`. This closure receives a `AsyncThrowingStream.Continuation`
  ///   instance that it uses to provide elements to the stream and terminate the
  ///   stream when finished.
  ///
  /// The `AsyncThrowingStream.Contuation` received by the `build` closure is
  /// appopriate for use in concurrent contexts. It is thread safe to send and
  /// finish; all calls are to the continuation are serialized, however calling
  /// this from multiple concurrent contexts could result in out of order
  ///  delivery.
  ///
  /// The following example shows an `AsyncThrowingStream` created with this
  /// initializer that produces random numbers on a one-second interval. When
  /// a private `keepRunning` variable becomes `false`, the inner `while` loop
  /// exits and the stream finishes. If the random number is divisble by 5 with
  /// no remainder, the stream throws a `MyRandomNumberError`.
  ///
  ///     let stream = AsyncThrowingStream<Int, Error>(
  ///         Int.self, bufferingPolicy: .bufferingNewest(5)) { continuation in
  ///             Task.detached {
  ///                 while (keepRunning) {
  ///                     await Task.sleep(1 * 1_000_000_000)
  ///                     let random = Int.random(in: 1...10)
  ///                     if (random % 5 == 0) {
  ///                         continuation.finish(throwing: MyRandomNumberError())
  ///                         break
  ///                     } else {
  ///                         continuation.yield(random)
  ///                     }
  ///                 }
  ///                 continuation.finish()
  ///             }
  ///         }
  ///
  ///     // call point:
  ///     do {
  ///         for try await random in stream {
  ///             print ("\(random)")
  ///         }
  ///     } catch {
  ///         print ("\(error)")
  ///     }
  ///
  public init(
    _ elementType: Element.Type = Element.self,
    bufferingPolicy limit: Continuation.BufferingPolicy = .unbounded,
    _ build: (Continuation) -> Void
  ) where Failure == Error {
    let storage: _Storage = .create(limit: limit)
    self.init(unfolding: storage.next)
    build(Continuation(storage: storage))
  }
  
  /// Constructs an asynchronous throwing stream from a given element-producing
  /// closure.
  ///
  /// - Parameters:
  ///   - produce: A closure that asynchronously produces elements for the
  ///    stream.
  ///
  /// Use this convenience initializer when you have an asychronous function
  /// that can produce elements for the stream, and don't want to invoke
  /// a continuation manually. This initializer "unfolds" your closure into
  /// a full-blown asynchronous stream. The created stream handles adherence to
  /// the `AsyncSequence` protocol automatically. To terminate the stream with
  /// an error, throw the error from your closure.
  ///
  /// The following example shows an `AsyncThrowingStream` created with this
  /// initializer that produces random numbers on a one-second interval. If the
  /// random number is divisble by 5 with no remainder, the stream throws a
  ///  `MyRandomNumberError`.
  ///
  ///     let stream = AsyncThrowingStream<Int, Error> {
  ///             await Task.sleep(1 * 1_000_000_000)
  ///             let random = Int.random(in: 1...10)
  ///             if (random % 5 == 0) {
  ///                 throw MyRandomNumberError()
  ///             }
  ///             return random
  ///         }
  ///
  ///     // call point:
  ///     do {
  ///         for try await random in stream {
  ///             print ("\(random)")
  ///         }
  ///     } catch {
  ///         print ("\(error)")
  ///     }
  ///
  public init(
    unfolding produce: @escaping () async throws -> Element?
  ) {
    self.produce = produce
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingStream: AsyncSequence {
  /// The asynchronous iterator for iterating an asynchronous stream.
  ///
  /// This type is specificially not `Sendable`. Do not use it from multiple
  /// concurrent contexts. It is a programmer error to invoke `next()` from a
  /// concurrent context that contends with another such call, and this will
  /// result in a call to `fatalError()`.
  public struct Iterator: AsyncIteratorProtocol {
    let produce: () async throws -> Element?

    /// The next value from the asynchronous stream.
    ///
    /// When `next()` returns `nil`, this signifies the end of the
    /// `AsyncStream`.
    ///
    /// It is a programmer error to invoke `next()` from a
    /// concurrent context that contends with another such call, and this will
    /// result in a call to `fatalError()`.
    ///
    /// If you cancel the task this iterator is running in while `next()` is
    /// awaiting a value, the `AsyncStream` terminates. In this case, `next()`
    /// may return `nil` immediately, or else return `nil` on subseuqent calls.
    public mutating func next() async throws -> Element? {
      return try await produce()
    }
  }

  /// Creates the asynchronous iterator that produces elements of this
  /// asynchronous sequence.
  public func makeAsyncIterator() -> Iterator {
    return Iterator(produce: produce)
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingStream.Continuation {
    
  /// Resume the task awaiting the next iteration point by having it return
  /// normally or throw, based on a given result.
  ///
  /// - Parameter result: A result to yield from the continuation. In the
  ///   `.success(_:)` case, this returns the associated value from the
  ///   iterator's `next()` method. If the result is the `failure(_:)` case,
  ///   this call terminates the stream with the result's error, by calling
  ///   `finish(throwing:)`.
  /// - Returns: A `YieldResult` indicating the success or failure of the
  ///   yield operation.
  ///
  /// If nothing is awaiting the next value and the result is success, this call
  /// attempts to buffer the result's element.
  ///
  /// If you call this method repeatedly, each call returns immediately, without
  /// blocking for any awaiting consumption from the iteration.
  @discardableResult
  public func yield(
    with result: Result<Element, Failure>
  ) -> YieldResult where Failure == Error {
    switch result {
    case .success(let val):
      return storage.yield(val)
    case .failure(let err):
      storage.finish(throwing: err)
      return .terminated
    }
  }

    /// Resume the task awaiting the next iteration point by having it return
    /// nomally from its suspension point.
    ///
    /// - Returns: A `YieldResult` indicating the success or failure of the
    ///   yield operation.
    ///
    /// Use this method with `AsyncStream` instances whose `Element` type is
    /// `Void`. In this case, the `yield()` call simply unblocks the awaiting
    /// iteration; there is no value to return.
    ///
    /// If you call this method repeatedly, each call returns immediately, without
    /// blocking for any awaiting consumption from the iteration.
  @discardableResult
  public func yield() -> YieldResult where Element == Void {
    storage.yield(())
  }
}
