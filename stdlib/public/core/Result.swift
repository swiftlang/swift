//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A value that represents either a success or failure, capturing associated
/// values in both cases.
@_frozen
public enum Result<Value, Error: Swift.Error> {
  /// A success, storing a `Value`.
  case value(Value)
  
  /// A failure, storing an `Error`.
  case error(Error)
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.value`, passing the value as a parameter.
  ///
  /// Use the `map` method with a closure that returns a non-`Result` value.
  ///
  /// - Parameter transform: A closure that takes the successful value of the
  ///   instance.
  /// - Returns: A new `Result` instance with the result of the transform, if
  ///   it was applied.
  public func map<NewValue>(
    _ transform: (Value) -> NewValue
  ) -> Result<NewValue, Error> {
    switch self {
    case let .value(value):
      return .value(transform(value))
    case let .error(error):
      return .error(error)
    }
  }
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.error`, passing the error as a parameter.
  ///
  /// Use the `mapError` method with a closure that returns a non-`Result`
  /// value.
  ///
  /// - Parameter transform: A closure that takes the failure value of the
  ///   instance.
  /// - Returns: A new `Result` instance with the result of the transform, if
  ///   it was applied.
  public func mapError<NewError>(
    _ transform: (Error) -> NewError
  ) -> Result<Value, NewError> {
    switch self {
    case let .value(value):
      return .value(value)
    case let .error(error):
      return .error(transform(error))
    }
  }
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.value`, passing the value as a parameter and flattening the result.
  ///
  /// - Parameter transform: A closure that takes the successful value of the
  ///   instance.
  /// - Returns: A new `Result` instance, either from the transform or from
  ///   the previous error value.
  public func flatMap<NewValue>(
    _ transform: (Value) -> Result<NewValue, Error>
  ) -> Result<NewValue, Error> {
    switch self {
    case let .value(value):
      return transform(value)
    case let .error(error):
      return .error(error)
    }
  }
  
  /// Evaluates the given transform closure when this `Result` instance is
  /// `.error`, passing the error as a parameter and flattening the result.
  ///
  /// - Parameter transform: A closure that takes the error value of the
  ///   instance.
  /// - Returns: A new `Result` instance, either from the transform or from
  ///   the previous success value.
  public func flatMapError<NewError>(
    _ transform: (Error) -> Result<Value, NewError>
  ) -> Result<Value, NewError> {
    switch self {
    case let .value(value):
      return .value(value)
    case let .error(error):
      return transform(error)
    }
  }
  
  /// Unwraps the `Result` into a throwing expression.
  ///
  /// - Returns: The success value, if the instance is a success.
  /// - Throws:  The error value, if the instance is a failure.
  public func unwrapped() throws -> Value {
    switch self {
    case let .value(value):
      return value
    case let .error(error):
      throw error
    }
  }
}

extension Result where Error == Swift.Error {
  /// Create an instance by capturing the output of a throwing closure.
  ///
  /// - Parameter catching: A throwing closure to evaluate.
  @_transparent
  public init(catching body: () throws -> Value) {
    do {
      let value = try body()
      self = .value(value)
    } catch {
      self = .error(error)
    }
  }
}

extension Result : Equatable where Value : Equatable, Error : Equatable { }

extension Result : Hashable where Value : Hashable, Error : Hashable {
  public func hash(into hasher: inout Hasher) {
    switch self {
    case let .value(value):
      hasher.combine(value)
      hasher.combine(Optional<Error>.none)
    case let .error(error):
      hasher.combine(Optional<Value>.none)
      hasher.combine(error)
    }
  }
}
