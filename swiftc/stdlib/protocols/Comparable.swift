//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A type that can be compared using the relational operators `<`, `<=`, `>=`,
/// and `>`.
///
/// The `Comparable` protocol is used for types that have an inherent order,
/// such as numbers and strings. Many types in the standard library already
/// conform to the `Comparable` protocol. Add `Comparable` conformance to your
/// own custom types when you want to be able to compare instances using
/// relational operators or use standard library methods that are designed for
/// `Comparable` types.
public protocol Comparable: Equatable {
  /// Returns a Boolean value indicating whether the value of the first
  /// argument is less than that of the second argument.
  ///
  /// This function is the only requirement of the `Comparable` protocol. The
  /// remainder of the relational operator functions are implemented by the
  /// standard library for any type that conforms to `Comparable`.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  static func < (lhs: Self, rhs: Self) -> Bool
}

// MARK: - Default implementations

extension Comparable {
  /// Returns a Boolean value indicating whether the value of the first
  /// argument is less than or equal to that of the second argument.
  @_transparent
  public static func <= (lhs: Self, rhs: Self) -> Bool {
    return lhs < rhs || lhs == rhs
  }

  /// Returns a Boolean value indicating whether the value of the first
  /// argument is greater than that of the second argument.
  @_transparent
  public static func > (lhs: Self, rhs: Self) -> Bool {
    return rhs < lhs
  }

  /// Returns a Boolean value indicating whether the value of the first
  /// argument is greater than or equal to that of the second argument.
  @_transparent
  public static func >= (lhs: Self, rhs: Self) -> Bool {
    return !(lhs < rhs)
  }
}

// MARK: - Convenience functions

/// Returns the lesser of two comparable values.
@_transparent
public func min<T>(_ x: T, _ y: T) -> T where T : Comparable {
  return y < x ? y : x
}

/// Returns the greater of two comparable values.
@_transparent
public func max<T>(_ x: T, _ y: T) -> T where T : Comparable {
  return y > x ? y : x
}