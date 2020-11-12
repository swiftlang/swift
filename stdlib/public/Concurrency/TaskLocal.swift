////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2020 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

import Darwin

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
}

extension Task {

  /// Read a task-local value, bound to the specified key.
  ///
  /// - Parameter keyPath: key path to the `TaskLocalKey` to be used for lookup
  /// - Returns: the value bound to the key, or its default value it if was not
  ///            bound in the current (or any parent) tasks.
  public static func local<Key>(_ keyPath: KeyPath<TaskLocalValues, Key>)
    async -> Key.Value where Key: TaskLocalKey {
    let task = Builtin.getCurrentAsyncTask()

    guard let rawValue = _taskLocalValueGet(task, keyType: Key.self) else {
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
  ///   - value:
  ///   - body:
  /// - Returns: the value returned by the `body` function.
  public static func withLocal<Key, BodyResult>(
    _ keyPath: KeyPath<TaskLocalValues, Key>,
    boundTo value: Key.Value,
    body: @escaping () async -> BodyResult
  ) async -> BodyResult where Key: TaskLocalKey {
    let task = Builtin.getCurrentAsyncTask()

//    fputs("error [Task.swift:\(#line)]: setting DONE keyType=\(Key.self), value=\(value)\n", stderr)
    _taskLocalValuePush(task, keyType: Key.self, value: value)
  
    defer {
      _taskLocalValuePop(task, count: 1)
    }

    return await body()
  }

  /// Bind the task local key to the given value for the scope of the `body` function.
  /// Any child tasks spawned within this scope will inherit the binding.
  ///
  /// - Parameters:
  ///   - key:
  ///   - value:
  ///   - body:
  /// - Returns: the value returned by the `body` function, or throws.
  public static func withLocal<Key, BodyResult>(
    _ keyPath: KeyPath<TaskLocalValues, Key>,
    boundTo value: Key.Value,
    body: @escaping () async throws -> BodyResult
  ) async throws -> BodyResult where Key: TaskLocalKey {
    let task = Builtin.getCurrentAsyncTask()

//    fputs("error [Task.swift:\(#line)]: setting DONE keyType=\(Key.self), value=\(value)\n", stderr)
    _taskLocalValuePush(task, keyType: Key.self, value: value)

    defer {
      _taskLocalValuePop(task, count: 1)
    }

    return try! await body()
  }
}

// ==== ------------------------------------------------------------------------

/// A type-erased `TaskLocalKey` used when iterating through the `Baggage` using its `forEach` method.
struct AnyTaskLocalKey {
  let keyType: Any.Type
  let valueType: Any.Type

  init<Key>(_: Key.Type) where Key: TaskLocalKey {
    self.keyType = Key.self
    self.valueType = Key.Value.self
  }
}

extension AnyTaskLocalKey: Hashable {
  static func ==(lhs: AnyTaskLocalKey, rhs: AnyTaskLocalKey) -> Bool {
    return ObjectIdentifier(lhs.keyType) == ObjectIdentifier(rhs.keyType)
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self.keyType))
  }
}

// ==== ------------------------------------------------------------------------

@_silgen_name("swift_task_localValuePush")
public func _taskLocalValuePush<Value>(
  _ task: Builtin.NativeObject,
  keyType: Any.Type/*Key.Type*/,
  value: __owned Value
)

@_silgen_name("swift_task_localValuePop")
public func _taskLocalValuePop(
  _ task: Builtin.NativeObject,
  count: Int
)

@_silgen_name("swift_task_localValueGet")
public func _taskLocalValueGet(
  _ task: Builtin.NativeObject,
  keyType: Any.Type/*Key.Type*/
) -> UnsafeMutableRawPointer? // where Key: TaskLocalKey

@_silgen_name("swift_task_hasTaskLocalValues")
public func _taskHasTaskLocalValues(
  _ task: Builtin.NativeObject
) -> Bool
