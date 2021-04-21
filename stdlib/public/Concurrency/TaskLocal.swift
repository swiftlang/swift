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

/// This type must be a `class` so it has a stable identity, that is used as key
/// value for lookups in the task local storage.
@propertyWrapper
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public final class TaskLocal<Value>: CustomStringConvertible {
  var defaultValue: Value
  var inherit: TaskLocalInheritance

  // Note: could overload with additional parameters to support other features
  public init(wrappedValue: Value) {
    self.defaultValue = wrappedValue
    self.inherit = .default
  }

  public init(wrappedValue: Value, inherit: TaskLocalInheritance) {
    self.defaultValue = wrappedValue
    self.inherit = inherit
  }

  public var wrappedValue: Value {
    get {
      return withUnsafeCurrentTask { task in
        guard let task = task else {
          return self.defaultValue
        }

        let value = _taskLocalValueGet(
            task._task, key: unsafeBitCast(self, to: Builtin.RawPointer.self), inheritance: inherit.rawValue)

        guard let rawValue = value else {
          return self.defaultValue
        }

        // Take the value; The type should be correct by construction
        let storagePtr =
            rawValue.bindMemory(to: Value.self, capacity: 1)
        return UnsafeMutablePointer<Value>(mutating: storagePtr).pointee
      }
    }

    @available(*, unavailable, message: "use ‘$myTaskLocal.withValue(_:do:)’ instead")
    set {
      fatalError("Illegal attempt to set a \(Self.self) value, use `withValue(...) { ... }` instead.")
    }
  }

  public var projectedValue: TaskLocal<Value> {
    self
  }

  public func withValue<R>(_ valueDuringBody: Value, do body: () async throws -> R) async rethrows -> R {
    // check if we're not trying to bind a value from an illegal context; this may crash
    _checkIllegalTaskLocalBindingWithinWithTaskGroup()

    // we need to escape the `_task` since the withUnsafeCurrentTask closure is not `async`.
    // this is safe, since we know the task will remain alive because we are running inside of it.
    let _task = withUnsafeCurrentTask { task in
      task!._task // !-safe, guaranteed to have task available inside async function
    }

    _taskLocalValuePush(_task, key: unsafeBitCast(self, to: Builtin.RawPointer.self), value: valueDuringBody)
    defer { _taskLocalValuePop(_task) }

    return try await body()
  }

  public var description: String {
    "\(Self.self)(\(wrappedValue))"
  }

}

/// A `TaskLocalKey` is used to identify, bind and get a task local value from
/// a `Task` in which a function is currently executing.
///
/// - SeeAlso: `Task.withLocal(_:boundTo:operation:)`
/// - SeeAlso: `TaskLocal(_:)`
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension TaskLocalKey {
  public static var inherit: TaskLocalInheritance { .default }
}

/// Determines task local value behavior in child tasks.
// TODO: should likely remain extensible
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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

// ==== ------------------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_localValuePush")
public func _taskLocalValuePush<Value>(
  _ task: Builtin.NativeObject,
  key: Builtin.RawPointer/*: Key*/,
  value: __owned Value
) // where Key: TaskLocal

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_localValuePop")
public func _taskLocalValuePop(
  _ task: Builtin.NativeObject
)

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_localValueGet")
public func _taskLocalValueGet(
  _ task: Builtin.NativeObject,
  key: Builtin.RawPointer/*Key*/,
  inheritance: UInt8/*TaskLocalInheritance*/
) -> UnsafeMutableRawPointer? // where Key: TaskLocal

// ==== Checks -----------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@usableFromInline
func _checkIllegalTaskLocalBindingWithinWithTaskGroup(file: String = #file,
                                                      line: UInt = #line) {
  if _taskHasTaskGroupStatusRecord() {
    file.withCString { _fileStart in
      _reportIllegalTaskLocalBindingWithinWithTaskGroup(
          _fileStart, file.count, true, line)
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@usableFromInline
@_silgen_name("swift_task_reportIllegalTaskLocalBindingWithinWithTaskGroup")
func _reportIllegalTaskLocalBindingWithinWithTaskGroup(
  _ _filenameStart: UnsafePointer<Int8>,
  _ _filenameLength: Int,
  _ _filenameIsASCII: Bool,
  _ _line: UInt)
