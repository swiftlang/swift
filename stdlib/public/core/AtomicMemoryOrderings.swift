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

/// Specifies the memory ordering semantics of an atomic load operation.
@frozen
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct AtomicLoadOrdering {
  // This struct works like a non-frozen enum whose cases aren't reorderable.

  @usableFromInline
  internal var _rawValue: Int

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
  @_transparent // Debug performance
  public static var relaxed: AtomicLoadOrdering {
    AtomicLoadOrdering(_rawValue: 0)
  }

  /// An acquiring load prevents the effects of subsequent load and store
  /// operations on the current thread from appearing to happen before the
  /// effect of the atomic operation itself.
  @_transparent // Debug performance
  public static var acquiring: AtomicLoadOrdering {
    AtomicLoadOrdering(_rawValue: 1)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicLoadOrdering: Equatable {
  @_transparent // Debug performance
  public static func ==(
    left: AtomicLoadOrdering,
    right: AtomicLoadOrdering
  ) -> Bool {
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
  @_transparent // Debug performance
  public static var relaxed: AtomicStoreOrdering {
    AtomicStoreOrdering(_rawValue: 0)
  }

  /// A releasing store prevents the effects of previous load and store
  /// operations on the current thread from appearing to happen after the effect
  /// of the atomic operation itself.
  @_transparent // Debug performance
  public static var releasing: AtomicStoreOrdering {
    AtomicStoreOrdering(_rawValue: 2)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicStoreOrdering: Equatable {
  @_transparent // Debug performance
  public static func ==(
    left: AtomicStoreOrdering,
    right: AtomicStoreOrdering
  ) -> Bool {
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
  @_transparent // Debug performance
  public static var relaxed: AtomicUpdateOrdering {
    AtomicUpdateOrdering(_rawValue: 0)
  }

  /// An acquiring load prevents the effects of subsequent load and store
  /// operations on the current thread from appearing to happen before the
  /// effect of the atomic operation itself.
  @_transparent // Debug performance
  public static var acquiring: AtomicUpdateOrdering {
    AtomicUpdateOrdering(_rawValue: 1)
  }

  /// A releasing store prevents the effects of previous load and store
  /// operations on the current thread from appearing to happen after the effect
  /// of the atomic operation itself.
  @_transparent // Debug performance
  public static var releasing: AtomicUpdateOrdering {
    AtomicUpdateOrdering(_rawValue: 2)
  }

  /// An barrier operation is a combination of `.acquiring` and `.releasing`; it
  /// prevents all neighboring load and store operations on the current thread
  /// from appearing to happen in a different order in relation to the atomic
  /// operation.
  @_transparent // Debug performance
  public static var barrier: AtomicUpdateOrdering {
    AtomicUpdateOrdering(_rawValue: 3)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUpdateOrdering: Equatable {
  @_transparent // Debug performance
  public static func ==(
    left: AtomicUpdateOrdering,
    right: AtomicUpdateOrdering
  ) -> Bool {
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
    case .barrier: return "barrier"
    default: return "AtomicUpdateOrdering(\(_rawValue))"
    }
  }
}
