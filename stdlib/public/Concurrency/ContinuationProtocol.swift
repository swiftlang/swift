import Swift

/// A value that can continue a suspended task.
public protocol _Continuation {
  /// The result type of the continuation.
  ///
  /// This is the type that will be returned normally from the continuation.
  associatedtype T

  /// The error type of the continuation.
  ///
  /// This is the the type of error that will be thrown from the continuation on
  /// failure. Typically, it is `Error` to allow for all types of thrown error.
  /// It can also be `Never` if a continuation is not failable.`
  associatedtype E: Error

  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// - Parameter value: The value to return from the continuation.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, then the attempt to resume
  /// the continuation again will trap.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  func resume(returning value: __owned T)

  /// Resume the task awaiting the continuation by having it throw an error
  /// from its suspension point.
  ///
  /// - Parameter error: The error to throw from the continuation.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, then the attempt to resume
  /// the continuation again will trap.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  func resume(throwing error: __owned E)
}

extension _Continuation {
  /// Resume the task awaiting the continuation by having it either
  /// return normally or throw an error based on the state of the given
  /// `Result` value.
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, then the attempt to resume
  /// the continuation again will trap.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  @_alwaysEmitIntoClient
  public func resume<Er: Error>(with result: Result<T, Er>) where E == Error {
    switch result {
      case .success(let val):
        self.resume(returning: val)
      case .failure(let err):
        self.resume(throwing: err)
    }
  }

  /// Resume the task awaiting the continuation by having it either
  /// return normally or throw an error based on the state of the given
  /// `Result` value.
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, then the attempt to resume
  /// the continuation again will trap.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  @_alwaysEmitIntoClient
  public func resume(with result: Result<T, E>) {
    switch result {
      case .success(let val):
        self.resume(returning: val)
      case .failure(let err):
        self.resume(throwing: err)
    }
  }

  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point.
  ///
  /// A continuation must be resumed exactly once. If the continuation has
  /// already been resumed through this object, then the attempt to resume
  /// the continuation again will trap.
  ///
  /// After `resume` enqueues the task, control is immediately returned to
  /// the caller. The task will continue executing when its executor is
  /// able to reschedule it.
  @_alwaysEmitIntoClient
  public func resume() where T == Void {
    self.resume(returning: ())
  }
}
