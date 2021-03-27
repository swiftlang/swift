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
@_implementationOnly import _SwiftConcurrencyShims

/// Namespace for declaring `TaskLocalKey`s.
public enum TaskLocalValues {}

/// A `TaskLocalKey` is used to identify, bind and get a task local value from
/// a `Task` in which a function is currently executing.
///
/// - SeeAlso: `Task.withLocal(_:boundTo:operation:)`
/// - SeeAlso: `Task.local(_:)`
public protocol TaskLocalKey {
  /// The type of `Value` uniquely identified by this key.
  associatedtype Value

  /// If a task local value is not present in a given context, its `defaultValue`
  /// will be returned instead.
  ///
  /// A common pattern is to use an `Optional<T>` type and use `nil` as default value,
  /// if the type itself does not have a good "undefined" or "zero" value that could
  /// be used here.
  static var defaultValue: Value { get }

  /// Allows configuring specialized inheritance strategies for task local values.
  ///
  /// By default, task local values are accessible by the current or any of its
  /// child tasks (with this rule applying recursively).
  ///
  /// Some, rare yet important, use-cases may require specialized inheritance
  /// strategies, and this property allows them to configure these for their keys.
  static var inherit: TaskLocalInheritance { get }
}

extension TaskLocalKey {
  public static var inherit: TaskLocalInheritance { .default }
}

/// Determines task local value behavior in child tasks.
// TODO: should likely remain extensible
public enum TaskLocalInheritance: UInt8, Equatable {
  /// The default inheritance strategy.
  ///
  /// Task local values whose keys are `default` inherited are available to the
  /// task which declared them, as well as recursively by any child tasks
  case `default` = 0

  /// Causes task local values to never be inherited.
  /// If the parent task has a value bound using this key, and a child task
  /// attempts to look up a value of that key, it will return `defaultValue`.
  case never = 1
}

extension Task {

  /// Read a task-local value, bound to the specified key.
  ///
  /// - Parameter keyPath: key path to the `TaskLocalKey` to be used for lookup
  /// - Returns: the value bound to the key, or its default value it if was not
  ///            bound in the current (or any parent) tasks.
  public static func local<Key>(_ keyPath: KeyPath<TaskLocalValues, Key>)
    -> Key.Value where Key: TaskLocalKey {
    guard let unsafeTask = Task.unsafeCurrent else {
      return Key.defaultValue
    }

    let value = _taskLocalValueGet(
      unsafeTask._task, keyType: Key.self, inheritance: Key.inherit.rawValue)
    guard let rawValue = value else {
      return Key.defaultValue
    }

    // Take the value; The type should be correct by construction
    let storagePtr =
      rawValue.bindMemory(to: Key.Value.self, capacity: 1)
    return UnsafeMutablePointer<Key.Value>(mutating: storagePtr).pointee
  }

  /// Bind the task local key to the given value for the scope of the `body` function.
  /// Any child tasks spawned within this scope will inherit the binding.
  ///
  /// - Parameters:
  ///   - keyPath: key path to the `TaskLocalKey` to be used for lookup
  ///   - value: value to bind the task local to for the scope of `operation`
  ///   - operation: the operation to run with the task local value bound
  /// - Returns: the value returned by the `body` function.
  public static func withLocal<Key, BodyResult>(
    _ keyPath: KeyPath<TaskLocalValues, Key>,
    boundTo value: Key.Value,
    operation: () async throws -> BodyResult
  ) async rethrows -> BodyResult where Key: TaskLocalKey {
    let _task = Task.unsafeCurrent!._task // !-safe, guaranteed to have task available inside async function

    _taskLocalValuePush(_task, keyType: Key.self, value: value)
    defer { _taskLocalValuePop(_task) }

    return try await operation()
  }

}

// ==== ------------------------------------------------------------------------

@_silgen_name("swift_task_localValuePush")
public func _taskLocalValuePush<Value>(
  _ task: Builtin.NativeObject,
  keyType: Any.Type/*Key.Type*/,
  value: __owned Value
) // where Key: TaskLocalKey

@_silgen_name("swift_task_localValuePop")
public func _taskLocalValuePop(
  _ task: Builtin.NativeObject
)

@_silgen_name("swift_task_localValueGet")
public func _taskLocalValueGet(
  _ task: Builtin.NativeObject,
  keyType: Any.Type/*Key.Type*/,
  inheritance: UInt8/*TaskLocalInheritance*/
) -> UnsafeMutableRawPointer? // where Key: TaskLocalKey
