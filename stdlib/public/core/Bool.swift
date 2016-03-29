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
// Bool Datatype and Supporting Operators
//===----------------------------------------------------------------------===//

/// A value type whose instances are either `true` or `false`.
///
/// `Bool` is the default type for Boolean values in Swift. Create instances of
/// `Bool` by using one of the Boolean literals `true` and `false` or by
/// assigning the result of a Boolean method or operation to a variable or
/// constant.
///
///     var godotHasArrived = false
///
///     let numbers = 1...5
///     let containsTen = numbers.contains(10)
///     print(containsTen)
///     // Prints "false"
///
///     let (a, b) == (100, 101)
///     let aFirst = a < b
///     print(aFirst)
///     // Prints "true"
///
/// Swift uses only simple Boolean values in conditional contexts to help avoid
/// accidental programming errors and to help maintain the clarity of each
/// control statement. Unlike other programming languages, in Swift integers
/// and strings cannot be used where a Boolean value is expected.
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
@_fixed_layout
public struct Bool {
  internal var _value: Builtin.Int1

  /// Creates an instance initialized to `false`.
  ///
  /// Don't call this initializer directly. Instead, use the Boolean literal
  /// `false` to create a new `Bool` instance.
  @_transparent
  public init() {
    let zero: Int8 = 0
    self._value = Builtin.trunc_Int8_Int1(zero._value)
  }

  @_versioned
  @_transparent
  internal init(_ v: Builtin.Int1) { self._value = v }
}

extension Bool : _BuiltinBooleanLiteralConvertible, BooleanLiteralConvertible {
  @_transparent
  public init(_builtinBooleanLiteral value: Builtin.Int1) {
    self._value = value
  }

  /// Creates an instance initialized to the specified Boolean literal.
  ///
  /// Don't directly call this initializer, which is used by the compiler when
  /// you use a Boolean literal. Instead, create a new `Bool` instance by
  /// using one of the Boolean literals `true` and `false`.
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
  @_transparent
  public init(booleanLiteral value: Bool) {
    self = value
  }
}

extension Bool : Boolean {
  @_transparent
  @warn_unused_result
  public func _getBuiltinLogicValue() -> Builtin.Int1 {
    return _value
  }

  /// This value expressed as a `Bool` instance; its value is identical to that
  /// of the current instance.
  @_transparent public var boolValue: Bool { return self }

  /// Creates an instance representing the given logical value.
  ///
  /// - Parameter value: The logical value for the new instance.
  public init<T : Boolean>(_ value: T) {
    self = value.boolValue
  }
}

extension Bool : CustomStringConvertible {
  /// A textual representation of the Boolean value.
  public var description: String {
    return self ? "true" : "false"
  }
}

// This is a magic entry point known to the compiler.
@_transparent
public // COMPILER_INTRINSIC
func _getBool(_ v: Builtin.Int1) -> Bool { return Bool(v) }

@_transparent
extension Bool : Equatable, Hashable {
  /// The hash value for the Boolean value.
  ///
  /// Two values that are equal always have equal hash values.
  ///
  /// - Note: The hash value is not guaranteed to be stable across different
  ///   invocations of the same program. Do not persist the hash value across
  ///   program runs.
  /// - SeeAlso: `Hashable`
  public var hashValue: Int {
    return self ? 1 : 0
  }
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//

/// Performs a logical NOT operation on a Boolean value.
///
/// The `!` (logical NOT) operator inverts a Boolean value. If the value is
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
@_transparent
@warn_unused_result
public prefix func !(a: Bool) -> Bool {
  return Bool(Builtin.xor_Int1(a._value, true._value))
}

@_transparent
@warn_unused_result
public func ==(lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.cmp_eq_Int1(lhs._value, rhs._value))
}
