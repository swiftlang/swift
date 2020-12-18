import Swift

@_silgen_name("swift_continuation_logFailedCheck")
internal func logFailedCheck(_ message: UnsafeRawPointer)

/// A wrapper class for `UnsafeContinuation` that logs misuses of the
/// continuation, logging a message if the continuation is resumed
/// multiple times, or if an object is destroyed without its continuation
/// ever being resumed.
///
/// Raw `UnsafeContinuation`, like other unsafe constructs, requires the
/// user to apply it correctly in order to maintain invariants. The key
/// invariant is that the continuation must be resumed exactly once,
/// and bad things happen if this invariant is not upheld--if a continuation
/// is abandoned without resuming the task, then the task will be stuck in
/// the suspended state forever, and conversely, if the same continuation is
/// resumed multiple times, it will put the task in an undefined state.
/// `UnsafeContinuation` avoids enforcing these invariants at runtime because
/// it aims to be a low-overhead mechanism for interfacing Swift tasks with
/// event loops, delegate methods, callbacks, and other non-`async` scheduling
/// mechanisms. However, during development, being able to verify that the
/// invariants are being upheld in testing is important.
///
/// `CheckedContinuation` is designed to be a drop-in API replacement for
/// `UnsafeContinuation` that can be used for testing purposes, at the cost
/// of an extra allocation and indirection for the wrapper object.
/// Changing a call of `withUnsafeContinuation` into a call of
/// `withCheckedContinuation` should be enough to obtain the extra checking
/// without further source modification in most circumstances.
public final class CheckedContinuation<T> {
  var continuation: UnsafeContinuation<T>?
  var function: String
  
  /// Initialize a `CheckedContinuation` wrapper around an
  /// `UnsafeContinuation`.
  ///
  /// In most cases, you should use `withCheckedContinuation` instead.
  /// You only need to initialize your own `CheckedContinuation<T>` if you
  /// already have an `UnsafeContinuation` you want to impose checking on.
  ///
  /// - Parameters:
  ///   - continuation: a fresh `UnsafeContinuation` that has not yet
  ///     been resumed. The `UnsafeContinuation` must not be used outside of
  ///     this object once it's been given to the new object.
  ///   - function: a string identifying the declaration that is the notional
  ///     source for the continuation, used to identify the continuation in
  ///     runtime diagnostics related to misuse of this continuation.
  public init(continuation: UnsafeContinuation<T>, function: String = #function) {
    self.continuation = continuation
    self.function = function
  }
  
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, then the attempt to resume
  /// the continuation again will be logged, but otherwise have no effect.
  public func resume(returning x: __owned T) {
    if let c = continuation {
      c.resume(returning: x)
      // Clear out the continuation so we don't try to resume again
      continuation = nil
    } else {
      logFailedCheck("SWIFT TASK CONTINUATION MISUSE: \(function) tried to resume its continuation more than once, returning \(x)!\n")
    }
  }
  
  /// Log if the object is deallocated before its continuation is resumed.
  deinit {
    if continuation != nil {
      logFailedCheck("SWIFT TASK CONTINUATION MISUSE: \(function) leaked its continuation!\n")
    }
  }
}

public func withCheckedContinuation<T>(
    function: String = #function,
    _ body: (CheckedContinuation<T>) -> Void
) async -> T {
  return await withUnsafeContinuation {
    body(CheckedContinuation(continuation: $0, function: function))
  }
}

/// A wrapper class for `UnsafeThrowingContinuation` that logs misuses of the
/// continuation, logging a message if the continuation is resumed
/// multiple times, or if an object is destroyed without its continuation
/// ever being resumed.
///
/// Raw `UnsafeThrowingContinuation`, like other unsafe constructs, requires the
/// user to apply it correctly in order to maintain invariants. The key
/// invariant is that the continuation must be resumed exactly once,
/// and bad things happen if this invariant is not upheld--if a continuation
/// is abandoned without resuming the task, then the task will be stuck in
/// the suspended state forever, and conversely, if the same continuation is
/// resumed multiple times, it will put the task in an undefined state.
/// `UnsafeThrowingContinuation` avoids enforcing these invariants at runtime because
/// it aims to be a low-overhead mechanism for interfacing Swift tasks with
/// event loops, delegate methods, callbacks, and other non-`async` scheduling
/// mechanisms. However, during development, being able to verify that the
/// invariants are being upheld in testing is important.
///
/// `CheckedThrowingContinuation` is designed to be a drop-in API replacement for
/// `UnsafeThrowingContinuation` that can be used for testing purposes, at the cost
/// of an extra allocation and indirection for the wrapper object.
/// Changing a call of `withUnsafeThrowingContinuation` into a call of
/// `withCheckedThrowingContinuation` should be enough to obtain the extra checking
/// without further source modification in most circumstances.
public final class CheckedThrowingContinuation<T> {
  var continuation: UnsafeThrowingContinuation<T>?
  var function: String
  
  /// Initialize a `CheckedThrowingContinuation` wrapper around an
  /// `UnsafeThrowingContinuation`.
  ///
  /// In most cases, you should use `withCheckedThrowingContinuation` instead.
  /// You only need to initialize your own `CheckedThrowingContinuation<T>` if you
  /// already have an `UnsafeThrowingContinuation` you want to impose checking on.
  ///
  /// - Parameters:
  ///   - continuation: a fresh `UnsafeThrowingContinuation` that has not yet
  ///     been resumed. The `UnsafeThrowingContinuation` must not be used outside of
  ///     this object once it's been given to the new object.
  ///   - function: a string identifying the declaration that is the notional
  ///     source for the continuation, used to identify the continuation in
  ///     runtime diagnostics related to misuse of this continuation.
  public init(continuation: UnsafeThrowingContinuation<T>, function: String = #function) {
    self.continuation = continuation
    self.function = function
  }
  
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, whether by `resume(returning:)`
  /// or by `resume(throwing:)`, then the attempt to resume
  /// the continuation again will be logged, but otherwise have no effect.
  public func resume(returning x: __owned T) {
    if let c = continuation {
      c.resume(returning: x)
      // Clear out the continuation so we don't try to resume again
      continuation = nil
    } else {
      logFailedCheck("SWIFT TASK CONTINUATION MISUSE: \(function) tried to resume its continuation more than once, returning \(x)!\n")
    }
  }
  
  /// Resume the task awaiting the continuation by having it throw an error
  /// from its suspension point.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, whether by `resume(returning:)`
  /// or by `resume(throwing:)`, then the attempt to resume
  /// the continuation again will be logged, but otherwise have no effect.
  public func resume(throwing x: __owned Error) {
    if let c = continuation {
      c.resume(throwing: x)
      // Clear out the continuation so we don't try to resume again
      continuation = nil
    } else {
      logFailedCheck("SWIFT TASK CONTINUATION MISUSE: \(function) tried to resume its continuation more than once, throwing \(x)!\n")
    }
  }
  
  /// Log if the object is deallocated before its continuation is resumed.
  deinit {
    if continuation != nil {
      logFailedCheck("SWIFT TASK CONTINUATION MISUSE: \(function) leaked its continuation!\n")
    }
  }
}

public func withCheckedThrowingContinuation<T>(
    function: String = #function,
    _ body: (CheckedThrowingContinuation<T>) -> Void
) async throws -> T {
  return await try withUnsafeThrowingContinuation {
    body(CheckedThrowingContinuation(continuation: $0, function: function))
  }
}

