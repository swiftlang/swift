//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import Swift


/// An error that indicates whether an operation failed due to deadline expiration or threw an error during
/// normal execution.
///
/// This error type distinguishes between two failure scenarios:
/// - The operation threw an error before the deadline expired.
/// - The operation was cancelled due to deadline expiration and then threw an error.
///
/// Use pattern matching to handle each case appropriately:
///
/// ```swift
/// let clock = ContinuousClock()
/// let deadline = clock.now.advanced(by: .seconds(5))
/// do {
///     let result = try await withDeadline(deadline) {
///         try await fetchDataFromServer()
///     }
///     print("Data received: \(result)")
/// } catch {
///     switch error.cause {
///     case .deadlineExpired:
///         print("Deadline exceeded and operation threw: \(error.underlyingError)")
///     case .operationFailed:
///         print("Operation failed before deadline: \(error.underlyingError)")
///     }
/// }
/// ```
public struct DeadlineError<OperationError: Error>: Error, CustomStringConvertible, CustomDebugStringConvertible {
  /// The underlying cause of the deadline error.
  public enum Cause: Sendable, CustomStringConvertible, CustomDebugStringConvertible {
    /// The operation was cancelled due to deadline expiration and subsequently threw an error.
    case deadlineExpired

    /// The operation threw an error before the deadline expired.
    case operationFailed
    
    public var description: String {
      switch self {
      case .deadlineExpired: ".deadlineExpired"
      case .operationFailed: ".operationFailed"
      }
    }
    
    public var debugDescription: String {
      description
    }
  }

  /// The underlying cause of the deadline error, indicating whether the operation
  /// failed before the deadline or was cancelled due to deadline expiration.
  public var cause: Cause

  /// The deadline expiration that was specified for the operation.
  public var expiration: any InstantProtocol


  /// The error thrown by the operation either in cases of expiration or failure
  public var underlyingError: OperationError

  /// Creates a deadline error with the specified cause and deadline expiration.
  public init(cause: Cause, expiration: any InstantProtocol, underlyingError: OperationError) {
    self.cause = cause
    self.expiration = expiration
    self.underlyingError = underlyingError
  }
  
  public var description: String {
    "DeadlineError(cause: \(cause), expiration: \(expiration), underlyingError: \(underlyingError)"
  }
  
  public var debugDescription: String {
    description
  }
}

class _TaskDeadline: @unchecked Sendable {
  var existentialExpiration: any InstantProtocol { fatalError() }
  var existentialClock: any Clock { fatalError() }
  
  func durationFromNow() -> Duration { fatalError() }
  
  func deadlineOffset(by duration: Duration) -> Self { fatalError() }
  
  func sleep(tolerance: Duration?) async throws { fatalError() }
}

final class TaskDeadline<C: Clock>: _TaskDeadline, @unchecked Sendable where C.Duration == Swift.Duration {
  let expiration: C.Instant
  let clock: C
  
  init(expiration: C.Instant, clock: C) {
    self.expiration = expiration
    self.clock = clock
  }
  
  override var existentialExpiration: any InstantProtocol {
    expiration
  }
  
  override var existentialClock: any Clock {
    clock
  }
  
  override func durationFromNow() -> Duration {
    clock.now.duration(to: expiration)
  }
  
  override func deadlineOffset(by duration: Duration) -> Self {
    Self(expiration: expiration.advanced(by: duration), clock: clock)
  }
  
  override func sleep(tolerance: Duration?) async throws {
    try await clock.sleep(until: expiration, tolerance: tolerance)
  }
}

@TaskLocal nonisolated var _currentDeadline = nil as _TaskDeadline?

extension Task where Success == Never, Failure == Never {
  nonisolated static var currentDeadline: _TaskDeadline? {
    _currentDeadline
  }
}

extension Optional where Wrapped: ~Copyable & ~Escapable {
	@_alwaysEmitIntoClient
  @_lifetime(copy self)
  mutating func takeSending() -> sending Self {
    let result = consume self
    self = nil
    return result
  }
}

// This is a helper type to move a non-Sendable value across isolation regions.
struct Disconnected<Value>: ~Copyable, Sendable {
  // This is safe since we take the value as sending and take consumes it
  // and returns it as sending.
  private nonisolated(unsafe) var value: Value?

  init(value: consuming sending Value) {
    self.value = .some(value)
  }

  consuming func take() -> sending Value {
    self.value.takeSending()!
  }
}

final class RefBox<Value: ~Copyable> {
  private nonisolated(unsafe) var value: Value?

  init(value: consuming Value) {
    self.value = consume value
  }

  consuming func unbox() -> Value {
    return value.take()!
  }
}

extension RefBox: Sendable where Value: Sendable & ~Copyable {}

private enum TaskResult<Return: Sendable, Failure: Error>: Sendable {
  case success(Return)
  case error(Failure)
  case timedOut
  case cancelled
}

func minimum<C: Clock>(_ left: _TaskDeadline, _ right: TaskDeadline<C>) -> _TaskDeadline where C.Instant.Duration == Swift.Duration {
  if let commonTypedLeft = left.existentialExpiration as? C.Instant {
    if commonTypedLeft < right.expiration {
      return left
    } else {
      // this cant just be returning the right, the clock itself has to be preserved (due to potential saved state)
      return TaskDeadline(expiration: right.expiration, clock: left.existentialClock as! C)
    }
  } else {
    // approximate as an offset from now
    let offset = right.clock.now.duration(to: right.expiration)
    let otherOffset = left.durationFromNow()
    if offset < otherOffset {
      return left.deadlineOffset(by: offset - otherOffset)
    } else {
      return left
    }
  }
}

nonisolated(nonsending) func __withDeadline<Return, Failure: Error, C: Clock>(
  clock: C,
  until expiration: C.Instant,
  tolerance: C.Instant.Duration?,
  body: inout (@Sendable () async throws(Failure) -> Return)?
) async throws(DeadlineError<Failure>) -> Return where C.Instant.Duration == Swift.Duration {
  let current = Task.currentDeadline
  let requested = TaskDeadline(expiration: expiration, clock: clock)
  let deadline = minimum(current ?? requested, requested)
  let result: Result<RefBox<Disconnected<Return>>, DeadlineError<Failure>> = await $_currentDeadline.withValue(deadline) {
    await withTaskGroup(
      of: TaskResult<RefBox<Disconnected<Return>>, Failure>.self
    ) { group in
      let body = body.takeSending()!
      group.addTask {
        do throws(Failure) {
          return .success(RefBox(value: Disconnected(value: try await body())))
        } catch {
          return .error(error)
        }
      }
      group.addTask {
        do {
          try await deadline.sleep(tolerance: tolerance)
          return .timedOut
        } catch {
          return .cancelled
        }
      }
      
      switch await group.next() {
      case .success(let result):
        // Work returned a result. Cancel the timer task and return
        group.cancelAll()
        return .success(result)
      case .error(let error):
        // Work threw before deadline. Cancel the timer task and throw operationFailed
        group.cancelAll()
        return .failure(DeadlineError(
          cause: .operationFailed,
          expiration: deadline.existentialExpiration,
          underlyingError: error
        ))
      case .timedOut:
        // Deadline exceeded, cancel the work task.
        group.cancelAll()
        
        switch await group.next() {
        case .success(let result):
          return .success(result)
        case .error(let error):
          return .failure(DeadlineError(
            cause: .deadlineExpired,
            expiration: deadline.existentialExpiration,
            underlyingError: error
          ))
        case .timedOut, .cancelled, .none:
          // We already got a result from the sleeping task so we can't get another one or none.
          fatalError("Unexpected task result")
        }
      case .cancelled:
        switch await group.next() {
        case .success(let result):
          return .success(result)
        case .error(let error):
          return .failure(DeadlineError(
            cause: .deadlineExpired,
            expiration: deadline.existentialExpiration,
            underlyingError: error
          ))
        case .timedOut, .cancelled, .none:
          // We already got a result from the sleeping task so we can't get another one or none.
          fatalError("Unexpected task result")
        }
      case .none:
        fatalError("Unexpected task result")
      }
    }
  }
  return try result.get().unbox().take()
}

nonisolated(nonsending) func _withDeadline<Return, Failure: Error, C: Clock>(
  clock: C,
  until deadline: C.Instant,
  tolerance: C.Instant.Duration?,
  body: @Sendable () async throws(Failure) -> Return
) async throws(DeadlineError<Failure>) -> Return where C.Instant.Duration == Swift.Duration  {
  try await withoutActuallyEscaping(body) { (escapingBody) async throws(DeadlineError<Failure>) -> Return in
    var t = Optional(escapingBody)
    return try await __withDeadline(clock: clock, until: deadline, tolerance: tolerance, body: &t)
  }
}


/// Perform an operation within a given deadline. If the deadline expires then the operation is cancelled.
/// At any point in which the operation throws an error a DeadlineError is thrown containing the failure
/// and information around the deadline being expired or not.
nonisolated(nonsending) public func withDeadline<Return, Failure: Error>(
  _ expiration: ContinuousClock.Instant,
  tolerance: ContinuousClock.Instant.Duration? = nil,
  body: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(DeadlineError<Failure>) -> Return {
  try await withDeadline(expiration, tolerance: tolerance, clock: ContinuousClock(), body: body)
}

/// Perform an operation within a given deadline. If the deadline expires then the operation is cancelled.
/// At any point in which the operation throws an error a DeadlineError is thrown containing the failure
/// and information around the deadline being expired or not.
nonisolated(nonsending) public func withDeadline<Return, Failure: Error, C: Clock>(
  _ expiration: C.Instant,
  tolerance: C.Instant.Duration? = nil,
  clock: C,
  body: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(DeadlineError<Failure>) -> Return where C.Instant.Duration == Swift.Duration {
  nonisolated(unsafe) let body = body
  return try await _withDeadline(
    clock: clock,
    until: expiration,
    tolerance: tolerance,
  ) { () async throws(Failure) -> Return in
    try await body()
  }
}

nonisolated(nonsending) public func withDeadline<Return, Failure: Error>(
  in timeout: ContinuousClock.Instant.Duration,
  tolerance: ContinuousClock.Instant.Duration? = nil,
  body: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(DeadlineError<Failure>) -> Return {
  try await withDeadline(.now + timeout, tolerance: tolerance, body: body)
}

nonisolated(nonsending) public func withDeadline<Return, Failure: Error, C: Clock>(
  in timeout: C.Instant.Duration,
  tolerance: C.Instant.Duration? = nil,
  clock: C,
  body: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(DeadlineError<Failure>) -> Return where C.Instant.Duration == Swift.Duration {
  try await withDeadline(clock.now.advanced(by: timeout), tolerance: tolerance, clock: clock, body: body)
}
