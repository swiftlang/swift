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

/// A type that can be hashed into a `Hasher` to produce an integer hash value.
///
/// You can use any type that conforms to the `Hashable` protocol in a set or as
/// a dictionary key. Many types in the standard library conform to `Hashable`:
/// Strings, integers, floating-point and Boolean values, and even sets are
/// hashable by default. Some other types, such as optionals, arrays and ranges
/// automatically become hashable when their type arguments implement the same.
///
/// Your own custom types can be hashable as well. When you define an
/// enumeration without associated values, it gains `Hashable` conformance
/// automatically, and you can add `Hashable` conformance to your other custom
/// types by implementing the `hash(into:)` method. For structs whose stored
/// properties are all `Hashable`, and for enum types that have all-`Hashable`
/// associated values, the compiler is able to provide an implementation of
/// `hash(into:)` automatically.
///
/// Hashing a value means feeding its essential components into a hash function,
/// represented by the `Hasher` type. Essential components are those that
/// contribute to the type's implementation of `Equatable`. Two instances that
/// are equal must feed the same values to `Hasher` in `hash(into:)`, in the
/// same order.
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
/// type to conform to `Hashable`, implement the `hash(into:)` function in your
/// custom type. To ensure that your type meets the semantic requirements of the
/// `Hashable` and `Equatable` protocols, it's a good idea to also customize
/// your type's `Equatable` conformance to match the `hash(into:)` definition.
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
/// `==` operator function and a `hash(into:)` method.
///
///     extension GridPoint: Hashable {
///         static func == (lhs: GridPoint, rhs: GridPoint) -> Bool {
///             return lhs.x == rhs.x && lhs.y == rhs.y
///         }
///
///         func hash(into hasher: inout Hasher) {
///             hasher.combine(x)
///             hasher.combine(y)
///         }
///     }
///
/// The `hash(into:)` method in this example feeds the properties `x` and `y`
/// to the supplied hasher; these are the same properties compared by the
/// implementation of the `==` operator function.
///
/// (Because `x` and `y` are both `Hashable` themselves, you could've also let
/// the compiler synthesize these implementations for you.)
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

  /// Hash the essential components of this value into the hash function
  /// represented by `hasher`, by feeding them into it using its `combine`
  /// methods.
  ///
  /// Essential components are precisely those that are compared in the type's
  /// implementation of `Equatable`.
  ///
  /// Note that `hash(into:)` doesn't own the hasher passed into it, so it must
  /// not call `finalize()` on it. Doing so may become a compile-time error in
  /// the future.
  func hash(into hasher: inout Hasher)
}

// Called by synthesized `hashValue` implementations.
@inlinable
@inline(__always)
public func _hashValue<H: Hashable>(for value: H) -> Int {
  var hasher = Hasher()
  hasher.combine(value)
  return hasher._finalize()
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
