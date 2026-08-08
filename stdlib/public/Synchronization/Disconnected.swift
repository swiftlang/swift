//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A wrapper that holds a value in a disconnected isolation region.
///
/// A value of `Disconnected<Value>` lives in a disconnected region: it has no
/// references to or from any other isolation region. That guarantee lets you
/// store such values in generic containers and later transfer them across
/// isolation boundaries without losing the information that the value was
/// in a disconnected region.
///
/// ## What is a disconnected region?
///
/// Region-based isolation partitions the values that exist at any point
/// during a program's execution into *isolation regions* based on which
/// references reach which storage. A value is in a *disconnected region*
/// when no reference reaches into or out of its storage from any other
/// region. Such a value is safe to transfer to a different isolation
/// region, such as an actor, a `Task`, or another concurrent context,
/// because the act of transferring it cannot create a data race.
///
/// In practice, a disconnected region typically arises from one of:
///
/// - A freshly constructed value whose initializer arguments were
///   themselves disconnected.
/// - A value that was just removed from another disconnected container.
/// - A `sending` parameter at a function boundary, which the callee
///   receives in a disconnected region.
/// - A `sending` return value from a function, which the caller receives
///   in a disconnected region.
///
/// The disconnected property is normally only tracked at `sending`
/// boundaries. `Disconnected<Value>` lets you preserve it across storage
/// boundaries (generic containers, stored properties, queues) that would
/// otherwise lose region information once the value is no longer at a
/// `sending` boundary.
///
/// ## Operations
///
/// Values enter the wrapper through ``init(_:)``, which requires a `sending`
/// argument. They leave through ``take()`` or ``swap(newValue:)``, both of
/// which return `sending Value`. ``withValue(body:)`` lends the wrapped value
/// in place to a closure that receives an `inout sending Value`.
///
/// Every operation either consumes the wrapper or replaces the wrapped value
/// through a `sending` boundary, so no alias into the wrapper's storage can
/// outlive a transfer. That property is what lets `Disconnected` conform to
/// `Sendable` regardless of whether `Value` itself conforms to `Sendable`.
///
/// ## Producing values that can cross isolation boundaries
///
/// Use ``take()`` to remove the wrapped value. The call consumes the
/// wrapper, so no further operations on it are possible. The returned
/// value is in a disconnected region, so you can transfer it across an
/// isolation boundary in the same expression, or store it and transfer it
/// later:
///
/// ```swift
/// final class Resource: ~Sendable {}
///
/// // `wrapper` was popped from a queue or other container holding
/// // `Disconnected<Resource>` values, so the resource it holds is
/// // already known to be disconnected from the surrounding context.
/// func process(wrapper: consuming Disconnected<Resource>) async {
///     let resource = wrapper.take()
///     await Task.detached {
///         use(resource) // OK: `resource` is in a disconnected region.
///     }.value
/// }
/// ```
///
/// Without the disconnected guarantee on the result, the captured `resource`
/// would be considered part of the caller's region and the capture in the
/// detached task would not be allowed.
///
/// ## Replacing the wrapped value in place
///
/// Use ``swap(newValue:)`` to exchange the held value for a new one in a
/// single step. The `newValue` argument is required to be in a disconnected
/// region. `swap` returns the previously stored value, which is in a
/// disconnected region:
///
/// ```swift
/// final class Resource: ~Sendable {}
///
/// func swapResources(in wrapper: inout Disconnected<Resource>) async {
///     let old = wrapper.swap(newValue: Resource())
///     await Task.detached {
///         dispose(old) // OK: `old` is in a disconnected region.
///     }.value
/// }
/// ```
///
/// Both directions of the swap cross a disconnected-region boundary: the
/// new value is required to be disconnected when it goes in, and the old
/// value is known to be disconnected when it comes out.
///
/// ## Mutating the wrapped value without taking it out
///
/// Use ``withValue(body:)`` when you need temporary mutable access without
/// removing the value. The closure receives the value as `inout sending
/// Value`, and `withValue` returns whatever `body` returns:
///
/// ```swift
/// var wrapper = Disconnected([Int]())
/// wrapper.withValue { array in
///     array.append(42)
/// }
/// ```
///
/// The `inout sending` parameter form means more than ordinary `inout`:
/// within the closure, the value can be transferred to another isolation
/// region, as long as the wrapper is left holding a disconnected value
/// when the closure returns. In typical use the closure performs an
/// in-place mutation; the more permissive shape is what makes `withValue`
/// composable with code that itself wants to send the value to another
/// isolation region.
@available(SwiftStdlib 6.5, *)
@frozen
@safe
public struct Disconnected<Value: ~Copyable>: ~Copyable, Sendable {
  // This is safe since the only values assigned are send into `Disconnected``.
  @usableFromInline
  internal nonisolated(unsafe) var _value: Value

  /// Creates a disconnected wrapper around the given value.
  ///
  /// The argument is required to be in a disconnected region at the call
  /// site. A freshly constructed value with no aliases satisfies this
  /// requirement directly:
  ///
  /// ```swift
  /// final class Resource: ~Sendable {}
  /// let wrapper = Disconnected(Resource())
  /// ```
  ///
  /// - Parameter value: The value to wrap. The wrapper takes ownership of
  ///   it.
  @available(SwiftStdlib 6.5, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ value: consuming sending Value) {
    self._value = value
  }

  /// Consumes the wrapper and returns the wrapped value.
  ///
  /// The returned value is in a disconnected region, so you can transfer it
  /// across an isolation boundary:
  ///
  /// ```swift
  /// let wrapper = Disconnected(Resource())
  /// let resource = wrapper.take()
  /// // `resource` can now be sent to another isolation region.
  /// ```
  ///
  /// After `take()` returns, the wrapper has been consumed and no further
  /// operations on it are possible.
  ///
  /// - Returns: The previously wrapped value, in a disconnected region.
  @available(SwiftStdlib 6.5, *)
  @_alwaysEmitIntoClient
  @_transparent
  public consuming func take() -> sending Value {
    let value = consume _value
    return value
  }

  /// Replaces the wrapped value with a new value and returns the previous
  /// one.
  ///
  /// `newValue` is required to be in a disconnected region. The previously
  /// stored value is returned and is in a disconnected region:
  ///
  /// ```swift
  /// var wrapper = Disconnected(Resource())
  /// let old = wrapper.swap(newValue: Resource())
  /// // `old` can now be sent to another isolation region.
  /// ```
  ///
  /// - Parameter newValue: The replacement value.
  /// - Returns: The previously wrapped value, in a disconnected region.
  @available(SwiftStdlib 6.5, *)
  @_alwaysEmitIntoClient
  @_transparent
  @discardableResult
  public mutating func swap(
    newValue: consuming sending Value
  ) -> sending Value {
    let old = consume _value
    self = Disconnected(newValue)
    return old
  }

  /// Calls `body` with mutable access to the wrapped value.
  ///
  /// The closure receives the value as `inout sending`, so within the
  /// closure scope the value can be transferred to another isolation
  /// region. The wrapper is required to hold a disconnected value once
  /// `body` returns.
  ///
  /// ```swift
  /// var wrapper = Disconnected([1, 2, 3])
  /// wrapper.withValue { array in
  ///     array.append(4)
  /// }
  /// ```
  ///
  /// If `body` throws, the wrapper retains whatever value the closure
  /// last left in storage and the error propagates to the caller.
  ///
  /// - Parameter body: A closure that receives `inout sending` access to
  ///   the wrapped value.
  /// - Returns: The value returned by `body`.
  /// - Throws: Any error thrown by `body`.
  @available(SwiftStdlib 6.5, *)
  @_alwaysEmitIntoClient
  @_transparent
  public mutating func withValue<Return: ~Copyable, Failure>(
    body: (inout sending Value) throws(Failure) -> Return
  ) throws(Failure) -> Return {
    var value = consume _value
    let result: Return
    do throws(Failure) {
      result = try body(&value)
    } catch {
      self = Disconnected(value)
      throw error
    }
    self = Disconnected(value)
    return result
  }
}
