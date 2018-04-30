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
// Bool Datatype and Supporting Operators
//===----------------------------------------------------------------------===//

/// A value type whose instances are either `true` or `false`.
///
/// `Bool` represents Boolean values in Swift. Create instances of `Bool` by
/// using one of the Boolean literals `true` or `false`, or by assigning the
/// result of a Boolean method or operation to a variable or constant.
///
///     var godotHasArrived = false
///
///     let numbers = 1...5
///     let containsTen = numbers.contains(10)
///     print(containsTen)
///     // Prints "false"
///
///     let (a, b) = (100, 101)
///     let aFirst = a < b
///     print(aFirst)
///     // Prints "true"
///
/// Swift uses only simple Boolean values in conditional contexts to help avoid
/// accidental programming errors and to help maintain the clarity of each
/// control statement. Unlike in other programming languages, in Swift, integers
/// and strings cannot be used where a Boolean value is required.
///
/// For example, the following code sample does not compile, because it
/// attempts to use the integer `i` in a logical context:
///
///     var i = 5
///     while i {
///         print(i)
///         i -= 1
///     }
///
/// The correct approach in Swift is to compare the `i` value with zero in the
/// `while` statement.
///
///     while i != 0 {
///         print(i)
///         i -= 1
///     }
///
/// Using Imported Boolean values
/// =============================
///
/// The C `bool` and `Boolean` types and the Objective-C `BOOL` type are all
/// bridged into Swift as `Bool`. The single `Bool` type in Swift guarantees
/// that functions, methods, and properties imported from C and Objective-C
/// have a consistent type interface.
@_fixed_layout
public struct Bool {
  @usableFromInline
  internal var _value: Builtin.Int1

  /// Creates an instance initialized to `false`.
  ///
  /// Do not call this initializer directly. Instead, use the Boolean literal
  /// `false` to create a new `Bool` instance.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init() {
    let zero: Int8 = 0
    self._value = Builtin.trunc_Int8_Int1(zero._value)
  }

  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal init(_ v: Builtin.Int1) { self._value = v }
  
  /// Creates an instance equal to the given Boolean value.
  ///
  /// - Parameter value: The Boolean value to copy.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ value: Bool) {
    self = value
  }
}

extension Bool : _ExpressibleByBuiltinBooleanLiteral, ExpressibleByBooleanLiteral {
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init(_builtinBooleanLiteral value: Builtin.Int1) {
    self._value = value
  }

  /// Creates an instance initialized to the specified Boolean literal.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you use a Boolean literal. Instead, create a new `Bool` instance by
  /// using one of the Boolean literals `true` or `false`.
  ///
  ///     var printedMessage = false
  ///
  ///     if !printedMessage {
  ///         print("You look nice today!")
  ///         printedMessage = true
  ///     }
  ///     // Prints "You look nice today!"
  ///
  /// In this example, both assignments to the `printedMessage` variable call
  /// this Boolean literal initializer behind the scenes.
  ///
  /// - Parameter value: The value of the new instance.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init(booleanLiteral value: Bool) {
    self = value
  }
}

extension Bool {
  // This is a magic entry point known to the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public // COMPILER_INTRINSIC
  func _getBuiltinLogicValue() -> Builtin.Int1 {
    return _value
  }
}

extension Bool : CustomStringConvertible {
  /// A textual representation of the Boolean value.
  @inlinable // FIXME(sil-serialize-all)
  public var description: String {
    return self ? "true" : "false"
  }
}

// This is a magic entry point known to the compiler.
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // COMPILER_INTRINSIC
func _getBool(_ v: Builtin.Int1) -> Bool { return Bool(v) }

extension Bool : Equatable, Hashable {
  /// The hash value for the Boolean value.
  ///
  /// Two values that are equal always have equal hash values.
  ///
  /// - Note: The hash value is not guaranteed to be stable across different
  ///   invocations of the same program. Do not persist the hash value across
  ///   program runs.
  @inlinable // FIXME(sil-serialize-all)
  public var hashValue: Int {
    return _hashValue(for: self)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func hash(into hasher: inout Hasher) {
    hasher.combine((self ? 1 : 0) as UInt8)
  }

  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public static func == (lhs: Bool, rhs: Bool) -> Bool {
    return Bool(Builtin.cmp_eq_Int1(lhs._value, rhs._value))
  }
}

extension Bool : LosslessStringConvertible {
  /// Creates a new Boolean value from the given string.
  ///
  /// If the `description` value is any string other than `"true"` or
  /// `"false"`, the result is `nil`. This initializer is case sensitive.
  ///
  /// - Parameter description: A string representation of the Boolean value.
  @inlinable // FIXME(sil-serialize-all)
  public init?(_ description: String) {
    if description == "true" {
      self = true
    } else if description == "false" {
      self = false
    } else {
      return nil
    }
  }
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//

extension Bool {
  /// Performs a logical NOT operation on a Boolean value.
  ///
  /// The logical NOT operator (`!`) inverts a Boolean value. If the value is
  /// `true`, the result of the operation is `false`; if the value is `false`,
  /// the result is `true`.
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
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public static prefix func ! (a: Bool) -> Bool {
    return Bool(Builtin.xor_Int1(a._value, true._value))
  }
}

extension Bool {
  /// Performs a logical AND operation on two Boolean values.
  ///
  /// The logical AND operator (`&&`) combines two Boolean values and returns
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
  /// - When `measurements.count` is greater than zero, `lhs` evaluates to
  ///   `true` and `rhs` is evaluated. The result of evaluating `rhs` is the
  ///   result of the `&&` operation.
  ///
  /// - Parameters:
  ///   - lhs: The left-hand side of the operation.
  ///   - rhs: The right-hand side of the operation.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  @inline(__always)
  public static func && (lhs: Bool, rhs: @autoclosure () throws -> Bool) rethrows
      -> Bool {
    return lhs ? try rhs() : false
  }

  /// Performs a logical OR operation on two Boolean values.
  ///
  /// The logical OR operator (`||`) combines two Boolean values and returns
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
  ///     // Prints "No major errors detected"
  ///
  /// In this example, `lhs` tests whether `error` is an empty string.
  /// Evaluation of the `||` operator is one of the following:
  ///
  /// - When `error` is an empty string, `lhs` evaluates to `true` and `rhs` is
  ///   not evaluated, skipping the call to `majorErrors.contains(_:)`. The
  ///   result of the operation is `true`.
  /// - When `error` is not an empty string, `lhs` evaluates to `false` and
  ///   `rhs` is evaluated. The result of evaluating `rhs` is the result of the
  ///   `||` operation.
  ///
  /// - Parameters:
  ///   - lhs: The left-hand side of the operation.
  ///   - rhs: The right-hand side of the operation.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  @inline(__always)
  public static func || (lhs: Bool, rhs: @autoclosure () throws -> Bool) rethrows
      -> Bool {
    return lhs ? true : try rhs()
  }
}

extension Bool {
  @inlinable
  /// Toggles the value of the Boolean. 
  ///
  /// Calling this method sets the variable to `true` if it was `false`,
  /// and sets it to `false` if it was `true`. For example:
  ///
  ///    var bools = [true, false]
  ///
  ///    bools[0].toggle()
  ///    // bools now contains [false, false]
  public mutating func toggle() {
    self = !self
  }
}
