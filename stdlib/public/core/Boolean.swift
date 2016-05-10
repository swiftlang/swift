//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Boolean
//===----------------------------------------------------------------------===//

/// Performs a logical NOT operation on a Boolean value.
///
/// The `!` (logical NOT) operator inverts a Boolean value. If the value is
/// `true`, the result of the operation is `false`; if the value is `false`,
/// the result is `true`. For example:
///
///     var printedMessage = false
///
///     if !printedMessage {
///         print("You look nice today!")
///         printedMessage = true
///     }
///     // Prints "You look nice today!"
///
/// - Parameter a: The Boolean value to negate.
@warn_unused_result
public prefix func !<T : Boolean>(a: T) -> Bool {
  return !a.boolValue
}

/// Performs a logical AND operation on two Boolean values.
///
/// The `&&` (logical AND) operator combines two Boolean values and returns
/// `true` if both of the values are `true`. If either of the values is
/// `false`, the operator returns `false`.
///
/// This operator uses short-circuit evaluation: The left-hand side (`lhs`) is
/// evaluated first, and the right-hand side (`rhs`) is evaluated only if
/// `lhs` evaluates to `true`. For example:
///
///     let measurements = [7.44, 6.51, 4.74, 5.88, 6.27, 6.12, 7.76]
///     let sum = measurements.reduce(0, combine: +)
///
///     if measurements.count > 0 && sum / Double(measurements.count) < 6.5 {
///         print("Average measurement is less than 6.5")
///     }
///     // Prints "Average measurement is less than 6.5"
///
/// In this example, `lhs` tests whether `measurements.count` is greater than
/// zero. Evaluation of the `&&` operator is one of the following:
///
/// - When `measurements.count` is equal to zero, `lhs` evaluates to `false`
///   and `rhs` is not evaluated, preventing a divide-by-zero error in the
///   expression `sum / Double(measurements.count)`. The result of the
///   operation is `false`.
/// - When `measurements.count` is greater than zero, `lhs` evaluates to `true`
///   and `rhs` is evaluated. The result of evaluating `rhs` is the result of
///   the `&&` operation.
///
/// - Parameters:
///   - lhs: The left-hand side of the operation.
///   - rhs: The right-hand side of the operation.
@inline(__always)
@warn_unused_result
public func && <T : Boolean, U : Boolean>(
  lhs: T, rhs: @autoclosure () throws -> U
) rethrows -> Bool {
  return lhs.boolValue ? try rhs().boolValue : false
}

/// Performs a logical OR operation on two Boolean values.
///
/// The `||` (logical OR) operator combines two Boolean values and returns
/// `true` if at least one of the values is `true`. If both values are
/// `false`, the operator returns `false`.
///
/// This operator uses short-circuit evaluation: The left-hand side (`lhs`) is
/// evaluated first, and the right-hand side (`rhs`) is evaluated only if
/// `lhs` evaluates to `false`. For example:
///
///     let majorErrors: Set = ["No first name", "No last name", ...]
///     let error = ""
///
///     if error.isEmpty || !majorErrors.contains(error) {
///         print("No major errors detected")
///     } else {
///         print("Major error: \(error)")
///     }
///     // Prints "No major errors detected")
///
/// In this example, `lhs` tests whether `error` is an empty string. Evaluation
/// of the `||` operator is one of the following:
///
/// - When `error` is an empty string, `lhs` evaluates to `true` and `rhs` is
///   not evaluated, skipping the call to `majorErrors.contains(_:)`. The
///   result of the operation is `true`.
/// - When `error` is not an empty string, `lhs` evaluates to `false` and `rhs`
///   is evaluated. The result of evaluating `rhs` is the result of the `||`
///   operation.
///
/// - Parameters:
///   - lhs: The left-hand side of the operation.
///   - rhs: The right-hand side of the operation.
@inline(__always)
@warn_unused_result
public func || <T : Boolean, U : Boolean>(
  lhs: T, rhs: @autoclosure () throws -> U
) rethrows -> Bool {
  return lhs.boolValue ? true : try rhs().boolValue
}

/// Performs a logical AND operation on two Boolean values.
///
/// The `&&` (logical AND) operator combines two Boolean values and returns
/// `true` if both of the values are `true`. If either of the values is
/// `false`, the operator returns `false`.
///
/// This operator uses short-circuit evaluation: The left-hand side (`lhs`) is
/// evaluated first, and the right-hand side (`rhs`) is evaluated only if
/// `lhs` evaluates to `true`. For example:
///
///     let measurements = [7.44, 6.51, 4.74, 5.88, 6.27, 6.12, 7.76]
///     let sum = measurements.reduce(0, combine: +)
///
///     if measurements.count > 0 && sum / Double(measurements.count) < 6.5 {
///         print("Average measurement is less than 6.5")
///     }
///     // Prints "Average measurement is less than 6.5"
///
/// In this example, `lhs` tests whether `measurements.count` is greater than
/// zero. Evaluation of the `&&` operator is one of the following:
///
/// - When `measurements.count` is equal to zero, `lhs` evaluates to `false`
///   and `rhs` is not evaluated, preventing a divide-by-zero error in the
///   expression `sum / Double(measurements.count)`. The result of the
///   operation is `false`.
/// - When `measurements.count` is greater than zero, `lhs` evaluates to `true`
///   and `rhs` is evaluated. The result of evaluating `rhs` is the result of
///   the `&&` operation.
///
/// - Parameters:
///   - lhs: The left-hand side of the operation.
///   - rhs: The right-hand side of the operation.
// FIXME: We can't make the above @_transparent due to
// rdar://problem/19418937, so here are some @_transparent overloads
// for Bool.  We've done the same for ObjCBool.
@_transparent
@warn_unused_result
public func && <T : Boolean>(
  lhs: T, rhs: @autoclosure () throws -> Bool
) rethrows -> Bool {
  return lhs.boolValue ? try rhs().boolValue : false
}

/// Performs a logical OR operation on two Boolean values.
///
/// The `||` (logical OR) operator combines two Boolean values and returns
/// `true` if at least one of the values is `true`. If both values are
/// `false`, the operator returns `false`.
///
/// This operator uses short-circuit evaluation: The left-hand side (`lhs`) is
/// evaluated first, and the right-hand side (`rhs`) is evaluated only if
/// `lhs` evaluates to `false`. For example:
///
///     let majorErrors: Set = ["No first name", "No last name", ...]
///     let error = ""
///
///     if error.isEmpty || !majorErrors.contains(error) {
///         print("No major errors detected")
///     } else {
///         print("Major error: \(error)")
///     }
///     // Prints "No major errors detected")
///
/// In this example, `lhs` tests whether `error` is an empty string. Evaluation
/// of the `||` operator is one of the following:
///
/// - When `error` is an empty string, `lhs` evaluates to `true` and `rhs` is
///   not evaluated, skipping the call to `majorErrors.contains(_:)`. The
///   result of the operation is `true`.
/// - When `error` is not an empty string, `lhs` evaluates to `false` and `rhs`
///   is evaluated. The result of evaluating `rhs` is the result of the `||`
///   operation.
///
/// - Parameters:
///   - lhs: The left-hand side of the operation.
///   - rhs: The right-hand side of the operation.
@_transparent
@warn_unused_result
public func || <T : Boolean>(
  lhs: T, rhs: @autoclosure () throws -> Bool
) rethrows -> Bool {
  return lhs.boolValue ? true : try rhs().boolValue
}
