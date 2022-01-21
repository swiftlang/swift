//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A type representing a future (i.e. an asynchronously-resolved `Result`.)
@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
public struct Future<Success: Sendable, Failure: Error>: Sendable {
  /// A type representing the current state of a future.
  fileprivate enum State {
    /// The future has not yet been scheduled (let alone resolved.)
    ///
    /// When a caller requests the result of a future for the first time, the
    /// associated closure is invoked.
    case pending(@Sendable () async -> Result<Success, Failure>)

    /// The future is currently (asynchronously) being resolved.
    ///
    /// The associated `Task` represents the work being done to resolve this
    /// future.
    case awaiting(Task<Result<Success, Failure>, Never>)

    /// The future is finished being resolved.
    ///
    /// The associated `Result` is, naturally, the result of the future.
    case finished(Result<Success, Failure>)
  }

  /// An actor type that manages concurrent access to a future.
  ///
  /// Instances of this type implement the basic logic of `Future`. `Future`
  /// itself is not an actor type so that it can instead be presented as a value
  /// type to callers.
  private actor _Impl {
    /// Storage for the associated future's state and eventual result.
    private var _state: State

    /// Initialize an instance of this type with the specified initial state.
    ///
    /// - Parameters:
    ///   - state: The initial state.
    init(_ state: State) {
      _state = state
    }

    /// Get the result of the associated future, potentially resolving it if
    /// needed.
    ///
    /// - Returns: The result of the associated future.
    func resolve() async -> Result<Success, Failure> {
      switch _state {
      case let .pending(body):
        // It's important to create a task here rather than just calling
        // `await body()`. The suspension point that is reached when `body` is
        // called is an opportunity for another thread/task to slip in, see that
        // `_state` equals `.pending`, and call `body` a second time.
        let task = Task { return await body() }
        _state = .awaiting(task)

        // The task is now scheduled and `self` is awaiting it. Recursively call
        // `self.resolve()` to await the task.
        return await self.resolve()

      case let .awaiting(task):
        // Await the running task. When it completes, update `_state` (assuming
        // another thread or task has not already done so.)
        let result = await task.value
        if case .awaiting = _state {
          _state = .finished(result)
        }
        return result

      case let .finished(result):
        // The associated future has already finished resolving and the result
        // is immediately available without needing to await it.
        return result
      }
    }
  }

  /// Storage for this future's implementation.
  private let _impl: _Impl

  /// Initialize an instance of this type with the specified initial state.
  ///
  /// - Parameters:
  ///   - state: The initial state.
  fileprivate init(_ state: State) {
    _impl = .init(state)
  }

  /// The result of this future.
  ///
  /// Getting the value of this property will schedule the future's
  /// corresponding work item (if it has not already been scheduled.)
  public var result: Result<Success, Failure> {
    get async {
      return await _impl.resolve()
    }
  }
}

// MARK: - Initializers

@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
extension Future {
  /// Initialize this future with a work item, `body`, that supplies its result.
  ///
  /// - Parameters:
  ///   - body: A work item to invoke asynchronously when a caller first needs
  ///     the future's result. `body` is not invoked if nothing ever requests
  ///     the future's result.
  public init(_ body: @escaping @Sendable () async -> Result<Success, Failure>) {
    self.init(.pending(body))
  }

  /// Initialize this future with a work item, `body`, that supplies its result.
  ///
  /// - Parameters:
  ///   - body: A work item to invoke asynchronously when a caller first needs
  ///     the future's result. `body` is not invoked if nothing ever requests
  ///     the future's result.
  public init(_ body: @escaping @Sendable () async -> Success) {
    self.init { return await .success(body()) }
  }

  /// Initialize this future with an existing task.
  ///
  /// - Parameters:
  ///   - task: An already-scheduled task. The result of the new future is equal
  ///     to `task.value`.
  public init(_ task: Task<Result<Success, Failure>, Never>) {
    self.init(.awaiting(task))
  }

  /// Initialize this future with an existing task.
  ///
  /// - Parameters:
  ///   - task: An already-scheduled task. The result of the new future is equal
  ///     to `task.result`.
  public init(_ task: Task<Success, Failure>) {
    self.init { return await task.result }
  }

  /// Initialize this future with an existing result.
  ///
  /// - Parameters:
  ///   - result: A result. The result of the new future is equal to this value.
  public init(_ result: Result<Success, Failure>) {
    self.init(.finished(result))
  }
}

// MARK: - Specializations

@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
extension Future where Failure == Error {
  /// Initialize this future with a work item, `body`, that supplies its result.
  ///
  /// - Parameters:
  ///   - body: A work item to invoke asynchronously when a caller first needs
  ///     the future's result. `body` is not invoked if nothing ever requests
  ///     the future's result.
  public init(_ body: @escaping @Sendable () async throws -> Success) {
    self.init(.pending {
      do {
        let success = try await body()
        return .success(success)
      } catch {
        return .failure(error)
      }
    })
  }

  /// Get the successful result of this future.
  ///
  /// - Throws: The failure value of this future's result if it did not succeed.
  ///
  /// Getting the value of this property will schedule the future's
  /// corresponding work item (if it has not already been scheduled.)
  public var value: Success {
    get async throws {
      return try await result.get()
    }
  }
}

@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
extension Future where Failure == Never {
  /// Initialize this future with a work item, `body`, that supplies its result.
  ///
  /// - Parameters:
  ///   - body: A work item to invoke asynchronously when a caller first needs
  ///     the future's result. `body` is not invoked if nothing ever requests
  ///     the future's result.
  public init(_ body: @escaping @Sendable () async -> Success) {
    // This initializer is the same as the generic one but is preferred when
    // type resolution is applied, so e.g. Future { return true } will naturally
    // produce a Future<Bool, Never> rather than a Future<Bool, Error>.
    self.init { return await .success(body()) }
  }

  /// The successful result of this future.
  ///
  /// Getting the value of this property will schedule the future's
  /// corresponding work item (if it has not already been scheduled.)
  public var value: Success {
    get async {
      return try! await result.get()
    }
  }
}

// MARK: - Functional interfaces

@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
extension Future {
  /// Lazily map this future, mapping any success value using the given
  /// transformation.
  ///
  /// - Parameters:
  ///   - transform: A closure that takes the success value of `self.result`
  ///     when it is resolved.
  ///
  /// - Returns: A new future that resolves to the mapped result of this future.
  ///
  /// Calling this function does not trigger the resolution of `self` or of the
  /// resulting future.
  public func map<NewSuccess: Sendable>(_ transform: @escaping @Sendable (_ success: Success) async -> NewSuccess) -> Future<NewSuccess, Failure> {
    return flatMap { return .init(.success(await transform($0))) }
  }

  /// Lazily map this future, mapping any failure value using the given
  /// transformation.
  ///
  /// - Parameters:
  ///   - transform: A closure that takes the failure value of `self.result`
  ///     when it is resolved.
  ///
  /// - Returns: A new future that resolves to the mapped result of this future.
  ///
  /// Calling this function does not trigger the resolution of `self` or of the
  /// resulting future.
  public func mapError<NewFailure: Error>(_ transform: @escaping @Sendable (_ failure: Failure) async -> NewFailure) -> Future<Success, NewFailure> {
    return flatMapError { return .init(.failure(await transform($0))) }
  }

  /// Lazily map this future, mapping any success value using the given
  /// transformation and unwrapping the produced future's result.
  ///
  /// - Parameters:
  ///   - transform: A closure that takes the success value of `self.result`
  ///     when it is resolved.
  ///
  /// - Returns: A new future that resolves to the mapped result of this future.
  ///
  /// Calling this function does not trigger the resolution of `self` or of the
  /// resulting future.
  public func flatMap<NewSuccess: Sendable>(_ transform: @escaping @Sendable (_ success: Success) async -> Future<NewSuccess, Failure>) -> Future<NewSuccess, Failure> {
    return .init { () async -> Result<NewSuccess, Failure> in
      switch await self.result {
      case let .success(success):
        return await transform(success).result
      case let .failure(failure):
        return .failure(failure)
      }
    }
  }

  /// Lazily map this future, mapping any failure value using the given
  /// transformation and unwrapping the produced future's result.
  ///
  /// - Parameters:
  ///   - transform: A closure that takes the failure value of `self.result`
  ///     when it is resolved.
  ///
  /// - Returns: A new future that resolves to the mapped result of this future.
  ///
  /// Calling this function does not trigger the resolution of `self` or of the
  /// resulting future.
  public func flatMapError<NewFailure: Error>(_ transform: @escaping @Sendable (_ failure: Failure) async -> Future<Success, NewFailure>) -> Future<Success, NewFailure> {
    return .init { () async -> Result<Success, NewFailure> in
      switch await self.result {
      case let .success(success):
        return .success(success)
      case let .failure(failure):
        return await transform(failure).result
      }
    }
  }
}
