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
  
  /// Evaluates the given closure when this `Result` instance is `.success`,
  /// passing the success value as a parameter.
  ///
  /// Use the `map` method with a closure that returns a non-`Result` value.
  ///
  /// - Parameter transform: A closure that takes the success value of the
  ///   instance.
  /// - Returns: A `Result` instance with the result of evaluating the given 
  ///   closure as the new success value if this instance is `.success`.
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
  
  /// Evaluates the given closure when this `Result` instance is `.failure`,
  /// passing the failure value as a parameter.
  ///
  /// Use the `mapError` method with a closure that returns a non-`Result`
  /// value.
  ///
  /// - Parameter transform: A closure that takes the failure value of the
  ///   instance.
  /// - Returns: A `Result` instance with the result of evaluating the given
  ///   closure as the new failure value if this instance is `.failure`.
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
  
  /// Evaluates the given closure when this `Result` instance is `.success`,
  /// passing the success value as a parameter.
  ///
  /// - Parameter transform: A closure that takes the success value of the
  ///   instance.
  /// - Returns: A`Result` instance, either from the closure or the previous 
  ///   `.failure`.
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
  
  /// Evaluates the given closure when this `Result` instance is `.failure`,
  /// passing the failure value as a parameter.
  ///
  /// - Parameter transform: A closure that takes the failure value of the
  ///   instance.
  /// - Returns: A `Result` instance, either from the closure or the previous 
  ///   `.success`.
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
  
  /// Get the success value as a throwing expression.
  ///
  /// - Returns: The success value, if the instance is `.success`.
  /// - Throws:  The failure value, if the instance is `.failure`.
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
  /// Create an instance by evaluating a throwing closure, capturing the 
  /// returned value as a `.success` or any thrown error as `.failure`.
  ///
  /// - Parameter catching: A throwing closure to evaluate.
  @_transparent
  public init(catching body: () throws -> Success) {
    do {
      self = .success(try body())
    } catch {
      self = .failure(error)
    }
  }
}

extension Result: Equatable where Success: Equatable, Failure: Equatable { }

extension Result: Hashable where Success: Hashable, Failure: Hashable { }
