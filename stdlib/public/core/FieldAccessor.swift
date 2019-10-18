//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A construct providing direct access to a stored property in instances of a
/// class type.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct FieldAccessor<Anchor: AnyObject, Value> {
  @usableFromInline
  internal let _offset: Int

  @usableFromInline
  internal init(_offset: Int) {
    self._offset = _offset
  }

  /// Initialize a new field accessor providing access to the stored property
  /// identified by the specified key path.
  public init(for key: ReferenceWritableKeyPath<Anchor, Value>) {
    guard let offset = key._storedInstanceOffset else {
      _preconditionFailure("FieldAccessor's key path isn't a stored property")
    }
    self._offset = offset
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension FieldAccessor {
  @inlinable
  internal func _pointer(in object: Anchor) -> UnsafeMutablePointer<Value> {
    let rawPtr = _getUnsafePointerToStoredProperty(
      atOffset: _offset,
      in: object)
    return rawPtr.assumingMemoryBound(to: Value.self)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension FieldAccessor {
  /// Calls the given closure with a direct pointer to the stored property
  /// inside the supplied object that is represented by this accessor.
  ///
  /// - Parameters:
  ///   - object: The object whose stored property needs to be accessed.
  ///   - body: A closure that takes a pointer to `Value` as its sole
  ///     argument. If the closure has a return value, that value is also used
  ///     as the return value of the `withUnsafeMutablePointer(in:_:)`
  ///     function. The pointer argument is valid only for the duration of the
  ///     function's execution.
  /// - Returns: The return value, if any, of the `body` closure.
  @inlinable
  public func withUnsafeMutablePointer<R>(
    in object: Anchor,
    _ body: (UnsafeMutablePointer<Value>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(_pointer(in: object))
  }
}
