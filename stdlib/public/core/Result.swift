//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A value that represents either a success or failure, capturing associated
/// values in both cases.
@_frozen
public enum Result<Success, Failure: Error> {
  /// A success, storing a `Success` value.
  case success(Success)
  
  /// A failure, storing a `Failure` value.
  case failure(Failure)
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.success`, passing the value as a parameter.
  ///
  /// Use the `map` method with a closure that returns a non-`Result` value.
  ///
  /// - Parameter transform: A closure that takes the successful value of the
  ///   instance.
  /// - Returns: A new `Result` instance with the result of the transform, if
  ///   it was applied.
  public func map<NewSuccess>(
    _ transform: (Success) -> NewSuccess
  ) -> Result<NewSuccess, Failure> {
    switch self {
    case let .success(success):
      return .success(transform(success))
    case let .failure(failure):
      return .failure(failure)
    }
  }
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.failure`, passing the error as a parameter.
  ///
  /// Use the `mapError` method with a closure that returns a non-`Result`
  /// value.
  ///
  /// - Parameter transform: A closure that takes the failure value of the
  ///   instance.
  /// - Returns: A new `Result` instance with the result of the transform, if
  ///   it was applied.
  public func mapError<NewFailure>(
    _ transform: (Failure) -> NewFailure
  ) -> Result<Success, NewFailure> {
    switch self {
    case let .success(success):
      return .success(success)
    case let .failure(failure):
      return .failure(transform(failure))
    }
  }
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.success`, passing the value as a parameter and flattening the result.
  ///
  /// - Parameter transform: A closure that takes the successful value of the
  ///   instance.
  /// - Returns: A new `Result` instance, either from the transform or from
  ///   the previous error value.
  public func flatMap<NewSuccess>(
    _ transform: (Success) -> Result<NewSuccess, Failure>
  ) -> Result<NewSuccess, Failure> {
    switch self {
    case let .success(success):
      return transform(success)
    case let .failure(failure):
      return .failure(failure)
    }
  }
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.failure`, passing the error as a parameter and flattening the result.
  ///
  /// - Parameter transform: A closure that takes the error value of the
  ///   instance.
  /// - Returns: A new `Result` instance, either from the transform or from
  ///   the previous success value.
  public func flatMapError<NewFailure>(
    _ transform: (Failure) -> Result<Success, NewFailure>
  ) -> Result<Success, NewFailure> {
    switch self {
    case let .success(success):
      return .success(success)
    case let .failure(failure):
      return transform(failure)
    }
  }
  
  /// Attempts to get the `success` value as a throwing expression.
  ///
  /// - Returns: The success value, if the instance is a success.
  /// - Throws:  The failure value, if the instance is a failure.
  public func get() throws -> Success {
    switch self {
    case let .success(success):
      return success
    case let .failure(failure):
      throw failure
    }
  }
}

extension Result where Failure == Swift.Error {
  /// Create an instance by capturing the output of a throwing closure.
  ///
  /// - Parameter catching: A throwing closure to evaluate.
  @_transparent
  public init(catching body: () throws -> Success) {
    do {
      let value = try body()
      self = .success(value)
    } catch {
      self = .failure(error)
    }
  }
}

extension Result : Equatable where Success : Equatable, Failure : Equatable { }

extension Result : Hashable where Success : Hashable, Failure : Hashable { }
