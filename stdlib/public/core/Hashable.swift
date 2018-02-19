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

/// A type that provides an integer hash value.
///
/// You can use any type that conforms to the `Hashable` protocol in a set or
/// as a dictionary key. Many types in the standard library conform to
/// `Hashable`: Strings, integers, floating-point and Boolean values, and even
/// sets provide a hash value by default. Your own custom types can be
/// hashable as well. When you define an enumeration without associated
/// values, it gains `Hashable` conformance automatically, and you can add
/// `Hashable` conformance to your other custom types by adding a single
/// `hashValue` property.
///
/// A hash value, provided by a type's `hashValue` property, is an integer that
/// is the same for any two instances that compare equally. That is, for two
/// instances `a` and `b` of the same type, if `a == b`, then
/// `a.hashValue == b.hashValue`. The reverse is not true: Two instances with
/// equal hash values are not necessarily equal to each other.
///
/// - Important: Hash values are not guaranteed to be equal across different
///   executions of your program. Do not save hash values to use in a future
///   execution.
///
/// Conforming to the Hashable Protocol
/// ===================================
///
/// To use your own custom type in a set or as the key type of a dictionary,
/// add `Hashable` conformance to your type. The `Hashable` protocol inherits
/// from the `Equatable` protocol, so you must also satisfy that protocol's
/// requirements.
///
/// A custom type's `Hashable` and `Equatable` requirements are automatically
/// synthesized by the compiler when you declare `Hashable` conformance in the
/// type's original declaration and your type meets these criteria:
///
/// - For a `struct`, all its stored properties must conform to `Hashable`.
/// - For an `enum`, all its associated values must conform to `Hashable`. (An
///   `enum` without associated values has `Hashable` conformance even without
///   the declaration.)
///
/// To customize your type's `Hashable` conformance, to adopt `Hashable` in a
/// type that doesn't meet the criteria listed above, or to extend an existing
/// type to conform to `Hashable`, implement the `hashValue` property in your
/// custom type. To ensure that your type meets the semantic requirements of
/// the `Hashable` and `Equatable` protocols, it's a good idea to also
/// customize your type's `Equatable` conformance to match.
///
/// As an example, consider a `GridPoint` type that describes a location in a
/// grid of buttons. Here's the initial declaration of the `GridPoint` type:
///
///     /// A point in an x-y coordinate system.
///     struct GridPoint {
///         var x: Int
///         var y: Int
///     }
///
/// You'd like to create a set of the grid points where a user has already
/// tapped. Because the `GridPoint` type is not hashable yet, it can't be used
/// as the `Element` type for a set. To add `Hashable` conformance, provide an
/// `==` operator function and a `hashValue` property.
///
///     extension GridPoint: Hashable {
///         var hashValue: Int {
///             return x.hashValue ^ y.hashValue &* 16777619
///         }
///
///         static func == (lhs: GridPoint, rhs: GridPoint) -> Bool {
///             return lhs.x == rhs.x && lhs.y == rhs.y
///         }
///     }
///
/// The `hashValue` property in this example combines the hash value of a grid
/// point's `x` property with the hash value of its `y` property multiplied by
/// a prime constant.
///
/// - Note: The above example above is a reasonably good hash function for a
///   simple type. If you're writing a hash function for a custom type, choose
///   a hashing algorithm that is appropriate for the kinds of data your type
///   comprises. Set and dictionary performance depends on hash values that
///   minimize collisions for their associated element and key types,
///   respectively.
///
/// Now that `GridPoint` conforms to the `Hashable` protocol, you can create a
/// set of previously tapped grid points.
///
///     var tappedPoints: Set = [GridPoint(x: 2, y: 3), GridPoint(x: 4, y: 1)]
///     let nextTap = GridPoint(x: 0, y: 1)
///     if tappedPoints.contains(nextTap) {
///         print("Already tapped at (\(nextTap.x), \(nextTap.y)).")
///     } else {
///         tappedPoints.insert(nextTap)
///         print("New tap detected at (\(nextTap.x), \(nextTap.y)).")
///     }
///     // Prints "New tap detected at (0, 1).")
public protocol Hashable : Equatable {
  /// The hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  var hashValue: Int { get }

  /// Feed bits to be hashed into the hash function represented by `hasher`.
  ///
  /// If this requirement is not explicitly implemented, the compiler
  /// automatically synthesizes an implementation for it.
  func _hash(into hasher: _UnsafeHasher) -> _UnsafeHasher
}

// Used in synthesized `hashValue` implementations.
@inline(__always)
public func _hashValue<H: Hashable>(for value: H) -> Int {
  var value = value
  return withUnsafePointer(to: &value) { _hashValue(for: $0) }
}

@_versioned
@inline(never)
@effects(readonly) // FIXME: Unjustified
internal func _hashValue<H: Hashable>(for pointer: UnsafePointer<H>) -> Int {
  var hasher = _Hasher()
  return withUnsafeMutablePointer(to: &hasher) { p in
    return pointer.pointee._hash(into: _UnsafeHasher(p))._finalized()
  }
}

// Called by the SwiftValue implementation.
@_silgen_name("_swift_stdlib_Hashable_isEqual_indirect")
internal func Hashable_isEqual_indirect<T : Hashable>(
  _ lhs: UnsafePointer<T>,
  _ rhs: UnsafePointer<T>
) -> Bool {
  return lhs.pointee == rhs.pointee
}

// Called by the SwiftValue implementation.
@_silgen_name("_swift_stdlib_Hashable_hashValue_indirect")
internal func Hashable_hashValue_indirect<T : Hashable>(
  _ value: UnsafePointer<T>
) -> Int {
  return value.pointee.hashValue
}

/// An unsafe wrapper around a stateful hash function, presenting a faux purely
/// functional interface to eliminate ARC overhead.
///
/// This is not a true value type; calling `appending` or `finalized` actually
/// mutates `self`'s state.
@_fixed_layout
public struct _UnsafeHasher {
  @_versioned
  internal let _rawState: UnsafeMutableRawPointer

  internal var _state: UnsafeMutablePointer<_Hasher> {
    @inline(__always)
    get { return _rawState.assumingMemoryBound(to: _Hasher.self) }
  }

  @inline(__always)
  @_versioned
  internal init(_ state: UnsafeMutablePointer<_Hasher>) {
    self._rawState = UnsafeMutableRawPointer(state)
  }

  @effects(readonly)
  @inline(never)
  public func appending(bitPattern value: Int) -> _UnsafeHasher {
    // The effects attribute is a lie; however, it enables the compiler to
    // eliminate unnecessary retain/releases protecting Hashable state around
    // calls to `_Hasher.append(_:)`.
    //
    // We don't have a way to describe the side-effects of an opaque function --
    // if it doesn't have an @effects attribute, the compiler has no choice but
    // to assume it may mutate the hashable we're visiting. We know that won't
    // be the case (the stdlib owns the hash function), but the only way to tell
    // this to the compiler is to pretend the state update is pure.
    _state.pointee._append(value)
    return self
  }

  @_inlineable
  @inline(__always)
  public func appending<H: Hashable>(_ value: H) -> _UnsafeHasher {
    return value._hash(into: self)
  }

  @inline(__always)
  internal func _appending(_ value: Int) -> _UnsafeHasher {
    _state.pointee._append(value)
    return self
  }

  @inline(__always)
  internal func _finalized() -> Int {
    return _state.pointee._finalize()
  }
}

// FIXME: This is purely for benchmarking; to be removed.
internal struct _QuickHasher {
  internal var _hash: Int

  @inline(__always)
  internal init() {
    _hash = 0
  }

  @inline(__always)
  internal mutating func _append(_ value: Int) {
    if _hash == 0 {
      _hash = value
      return
    }
    _hash = _combineHashValues(_hash, value)
  }

  @inline(__always)
  internal mutating func _finalize() -> Int {
    return _mixInt(_hash)
  }
}

internal typealias _Hasher = _QuickHasher
