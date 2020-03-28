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

/// Specifies the memory ordering semantics of an atomic load operation.
@frozen
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct AtomicLoadOrdering {
  // This struct works like a non-frozen enum whose cases aren't reorderable.

  @usableFromInline
  internal var _rawValue: Int

  @_semantics("constant_evaluable")
  @inlinable @_transparent // Debug performance
  internal init(_rawValue: Int) {
    self._rawValue = _rawValue
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicLoadOrdering {
  // FIXME: Explain these ordering levels in more detail.

  /// Guarantees the atomicity of the specific operation on which it is applied,
  /// but imposes no ordering constraints on any other reads or writes.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var relaxed: Self {
    Self(_rawValue: 0)
  }

  /// An acquiring load prevents the effects of subsequent load and store
  /// operations on the current thread from appearing to happen before the
  /// effect of the atomic operation itself.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var acquiring: Self {
    Self(_rawValue: 1)
  }

  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var sequentiallyConsistent: Self {
    Self(_rawValue: 4)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicLoadOrdering: Equatable {
  @_transparent // Debug performance
  public static func ==(left: Self, right: Self) -> Bool {
    return left._rawValue == right._rawValue
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicLoadOrdering: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_rawValue)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicLoadOrdering: CustomStringConvertible {
  public var description: String {
    switch self {
    case .relaxed: return "relaxed"
    case .acquiring: return "acquiring"
    case .sequentiallyConsistent: return "sequentiallyConsistent"
    default: return "AtomicLoadOrdering(\(_rawValue))"
    }
  }
}

//------------------------------------------------------------------------------

/// Specifies the memory ordering semantics of an atomic store operation.
@frozen
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct AtomicStoreOrdering {
  // This struct works like a non-frozen enum whose cases aren't reorderable.

  @usableFromInline
  internal var _rawValue: Int

  @_semantics("constant_evaluable")
  @inlinable @_transparent // Debug performance
  internal init(_rawValue: Int) {
    self._rawValue = _rawValue
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicStoreOrdering {
  // FIXME: Explain these ordering levels in more detail.

  /// Guarantees the atomicity of the specific operation on which it is applied,
  /// but imposes no ordering constraints on any other reads or writes.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var relaxed: Self {
    Self(_rawValue: 0)
  }

  /// A releasing store prevents the effects of previous load and store
  /// operations on the current thread from appearing to happen after the effect
  /// of the atomic operation itself.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var releasing: Self {
    Self(_rawValue: 2)
  }

  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var sequentiallyConsistent: Self {
    Self(_rawValue: 4)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicStoreOrdering: Equatable {
  @_transparent // Debug performance
  public static func ==(left: Self, right: Self) -> Bool {
    return left._rawValue == right._rawValue
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicStoreOrdering: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_rawValue)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicStoreOrdering: CustomStringConvertible {
  public var description: String {
    switch self {
    case .relaxed: return "relaxed"
    case .releasing: return "releasing"
    case .sequentiallyConsistent: return "sequentiallyConsistent"
    default: return "AtomicStoreOrdering(\(_rawValue))"
    }
  }
}

//------------------------------------------------------------------------------

/// Specifies the memory ordering semantics of an atomic read-modify-write
/// operation.
@frozen
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct AtomicUpdateOrdering {
  // This struct works like a non-frozen enum whose cases aren't reorderable.

  @usableFromInline
  internal var _rawValue: Int

  @_semantics("constant_evaluable")
  @inlinable @_transparent // Debug performance
  internal init(_rawValue: Int) {
    self._rawValue = _rawValue
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUpdateOrdering {
  // FIXME: Explain these ordering levels in more detail.

  /// Guarantees the atomicity of the specific operation on which it is applied,
  /// but imposes no ordering constraints on any other reads or writes.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var relaxed: Self {
    Self(_rawValue: 0)
  }

  /// An acquiring load prevents the effects of subsequent load and store
  /// operations on the current thread from appearing to happen before the
  /// effect of the atomic operation itself.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var acquiring: Self {
    Self(_rawValue: 1)
  }

  /// A releasing store prevents the effects of previous load and store
  /// operations on the current thread from appearing to happen after the effect
  /// of the atomic operation itself.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var releasing: Self {
    Self(_rawValue: 2)
  }

  /// An acquiring-and-releasing operation is a combination of `.acquiring` and
  /// `.releasing`; it prevents all neighboring load and store operations on the
  /// current thread from appearing to happen in a different order in relation
  /// to the atomic operation.
  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var acquiringAndReleasing: Self {
    Self(_rawValue: 3)
  }

  @_semantics("constant_evaluable")
  @_alwaysEmitIntoClient
  @_transparent // Debug performance
  public static var sequentiallyConsistent: Self {
    Self(_rawValue: 4)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUpdateOrdering: Equatable {
  @_transparent // Debug performance
  public static func ==(left: Self, right: Self) -> Bool {
    return left._rawValue == right._rawValue
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUpdateOrdering: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_rawValue)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUpdateOrdering: CustomStringConvertible {
  public var description: String {
    switch self {
    case .relaxed: return "relaxed"
    case .acquiring: return "acquiring"
    case .releasing: return "releasing"
    case .acquiringAndReleasing: return "acquiringAndReleasing"
    case .sequentiallyConsistent: return "sequentiallyConsistent"
    default: return "AtomicUpdateOrdering(\(_rawValue))"
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_transparent // Debug performance
public func atomicMemoryFence(
  ordering: AtomicUpdateOrdering
) {
  switch ordering {
  case .relaxed: break
  case .acquiring: Builtin.fence_acquire()
  case .releasing: Builtin.fence_release()
  case .acquiringAndReleasing: Builtin.fence_acqrel()
  default: Builtin.fence_seqcst()
  }
}
