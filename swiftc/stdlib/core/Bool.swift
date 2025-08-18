//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A value type whose instances are either `true` or `false`.
///
/// `Bool` represents Boolean values in Swift. Create instances of `Bool` by
/// using one of the Boolean literals `true` or `false`, or by assigning the
/// result of a Boolean method or operation to a variable or constant.
///
///     var isReady = false
///     let numbers = 1...5
///     let containsTen = numbers.contains(10)
///     print(containsTen)  // Prints "false"
///
/// Swift uses only simple Boolean values in conditional contexts to help avoid
/// accidental programming errors and to help maintain the clarity of each
/// control statement. Unlike in other programming languages, in Swift, integers
/// and strings cannot be used where a Boolean value is required.
@frozen
public struct Bool {
  internal var _value: Builtin.Int1

  /// Creates an instance initialized to `false`.
  ///
  /// Do not call this initializer directly. Instead, use the Boolean literal
  /// `false` to create a new `Bool` instance.
  @_transparent
  public init() {
    let zero: Builtin.Int1 = Builtin.zextOrBitCast_Int8_Int1(0._value)
    self._value = zero
  }

  @_transparent
  internal init(_ value: Builtin.Int1) {
    self._value = value
  }
}

extension Bool: CustomStringConvertible {
  /// A textual representation of the Boolean value.
  public var description: String {
    return self ? "true" : "false"
  }
}

extension Bool: TextOutputStreamable {
  /// Writes a textual representation of the Boolean value into the given
  /// output stream.
  public func write<Target>(to target: inout Target)
    where Target : TextOutputStream {
    target.write(description)
  }
}

extension Bool: Equatable {
  /// Returns a Boolean value indicating whether two values are equal.
  ///
  /// Equality is the inverse of inequality. For any values `a` and `b`,
  /// `a == b` implies that `a != b` is `false`.
  @_transparent
  public static func == (lhs: Bool, rhs: Bool) -> Bool {
    return Bool(Builtin.cmp_eq_Int1(lhs._value, rhs._value))
  }
}

extension Bool: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  public func hash(into hasher: inout Hasher) {
    hasher.combine(self ? 1 : 0)
  }
}

extension Bool: ExpressibleByBooleanLiteral {
  /// The type of the Boolean literal, `Bool`.
  public typealias BooleanLiteralType = Bool

  /// Creates an instance initialized to the specified Boolean literal.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you use a Boolean literal. Instead, create a new `Bool` instance by
  /// using one of the Boolean literals `true` or `false`.
  ///
  ///     var printedMessage = false
  ///
  ///     if !printedMessage {
  ///         print("Hello, world!")
  ///         printedMessage = true
  ///     }
  ///     // Prints "Hello, world!"
  @_transparent
  public init(booleanLiteral value: Bool) {
    self = value
  }
}

// MARK: - Logical operations

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
  ///         print("Hello, world!")
  ///         printedMessage = true
  ///     }
  ///     // Prints "Hello, world!"
  ///
  /// - Parameter a: The Boolean value to negate.
  @_transparent
  public static prefix func ! (a: Bool) -> Bool {
    return Bool(Builtin.xor_Int1(a._value, true._value))
  }
}

// MARK: - Conditional compilation support

extension Bool {
  /// Creates a Boolean value from a conditional compilation flag.
  ///
  /// This initializer is used internally by the compiler to evaluate
  /// conditional compilation blocks.
  @_transparent
  public init(_builtinBooleanLiteral value: Builtin.Int1) {
    self._value = value
  }
}

// MARK: - Random support

extension Bool: CaseIterable {
  /// A collection containing all values of this type.
  public static var allCases: [Bool] {
    return [false, true]
  }
}

// MARK: - Boolean literal constants

/// The Boolean literal `true`.
@_transparent
public var `true`: Bool {
  return Bool(_builtinBooleanLiteral: Builtin.int_true())
}

/// The Boolean literal `false`.
@_transparent
public var `false`: Bool {
  return Bool(_builtinBooleanLiteral: Builtin.int_false())
}