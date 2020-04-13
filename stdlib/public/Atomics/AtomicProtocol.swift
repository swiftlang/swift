//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public protocol AtomicProtocol {
  associatedtype _AtomicStorage: _PrimitiveAtomic

  /// Convert `value` to its atomic storage representation. Note that the act of
  /// conversion may have side effects (such as retaining a strong reference),
  /// so the returned value must be used to initialize exactly one atomic
  /// storage location.
  ///
  /// Each call to `prepareAtomicStorage(for:)` must be paired with call to
  /// `disposeAtomicStorage(at:)` to undo these potential side effects.
  ///
  /// Between initialization and the call to `disposeAtomicStorage`, the
  /// memory location must only be accessed through the atomic operations
  /// exposed by this type.
  ///
  /// For example, here is how these methods can be used to create a temporary
  /// atomic variable for the duration of a closure call:
  ///
  ///     extension AtomicProtocol {
  ///        mutating func withTemporaryAtomicValue(
  ///           _ body: (UnsafeAtomic<Self>) -> Void
  ///        ) {
  ///           let storage =
  ///              UnsafeMutablePointer<_AtomicStorage>.allocate(capacity: 1)
  ///           storage.initialize(to: Self._prepareAtomicStorage(for: self)
  ///           defer {
  ///              Self._disposeAtomicStorage(at: storage)
  ///              storage.deallocate()
  ///           }
  ///           let tmp = UnsafeAtomic<Self>(at: storage)
  ///           body(tmp)
  ///           self = tmp.load(ordering: .relaxed)
  ///        }
  ///     }
  ///
  ///     42.withTemporaryAtomicValue { atomic in
  ///        print(atomic.load(ordering: .relaxed) // Prints 42
  ///     }
  ///
  /// - Parameter value: The value to convert.
  /// - Returns: The atomic storage representation of `value`.
  static func _prepareAtomicStorage(for value: __owned Self) -> _AtomicStorage

  /// Deinitialize atomic storage at the specified memory location.
  ///
  /// The specified address must have been previously initialized with a value
  /// returned from `prepareAtomicStorage(for:)`, and after initialization, it
  /// must have only been accessed using the atomic operations provided.
  ///
  /// - Parameter storage: A storage value previously initialized with a value
  ///   returned by `prepareAtomicStorage(for:)`.
  static func _disposeAtomicStorage(_ storage: inout _AtomicStorage) -> Self

  static func _encodeAtomicStorage(for value: __owned Self) -> _AtomicStorage
  static func _decodeAtomicStorage(_ storage: __owned _AtomicStorage) -> Self

}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicProtocol where
  Self: RawRepresentable,
  RawValue: AtomicProtocol,
  _AtomicStorage == RawValue._AtomicStorage
{
  @inlinable
  public static func _prepareAtomicStorage(
    for value: __owned Self
  ) -> RawValue._AtomicStorage {
    return RawValue._prepareAtomicStorage(for: value.rawValue)
  }

  @inlinable
  public static func _disposeAtomicStorage(
    _ storage: inout RawValue._AtomicStorage
  ) -> Self {
    return Self(rawValue: RawValue._disposeAtomicStorage(&storage))!
  }

  @inlinable
  public static func _encodeAtomicStorage(
    for value: __owned Self
  ) -> RawValue._AtomicStorage {
    return RawValue._encodeAtomicStorage(for: value.rawValue)
  }

  @inlinable
  public static func _decodeAtomicStorage(
    _ storage: __owned RawValue._AtomicStorage
  ) -> Self {
    return Self(rawValue: RawValue._decodeAtomicStorage(storage))!
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicProtocol where _AtomicStorage == Self {
  @_transparent @_alwaysEmitIntoClient
  public static func _prepareAtomicStorage(for value: __owned Self) -> Self {
    value
  }
  @_transparent @_alwaysEmitIntoClient
  public static func _disposeAtomicStorage(_ storage: inout Self) -> Self {
    storage
  }

  @_transparent @_alwaysEmitIntoClient
  public static func _encodeAtomicStorage(for value: __owned Self) -> Self {
    value
  }
  @_transparent @_alwaysEmitIntoClient
  public static func _decodeAtomicStorage(_ storage: __owned Self) -> Self {
    storage
  }
}
