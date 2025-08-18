//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A type that represents either a wrapped value or `nil`, the absence of a value.
///
/// You use the `Optional` type whenever you use optional values, even if you
/// never type the word `Optional`. Swift's type system usually shows the
/// wrapped type's name with a trailing question mark (`?`) instead of showing
/// the full type name. For example, if a variable has the type `Int?`, that's
/// just another way of writing `Optional<Int>`.
@frozen
public enum Optional<Wrapped> {
  /// The absence of a value.
  ///
  /// In code, the absence of a value is typically written using the `nil`
  /// literal rather than the explicit `.none` enumeration case.
  case none

  /// The presence of a value, stored as `Wrapped`.
  case some(Wrapped)

  /// Creates an instance that stores the given value.
  @_transparent
  public init(_ some: Wrapped) {
    self = .some(some)
  }
}

// MARK: - ExpressibleByNilLiteral

extension Optional: ExpressibleByNilLiteral {
  /// Creates an instance initialized with `nil`.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you initialize an `Optional` instance with a `nil` literal. For example:
  ///
  ///     var i: Int? = nil
  ///
  /// In this example, the assignment to the `i` variable calls this
  /// initializer behind the scenes.
  @_transparent
  public init(nilLiteral: ()) {
    self = .none
  }
}

// MARK: - Unwrapping and nil-coalescing

extension Optional {
  /// Evaluates the given closure when this `Optional` instance is not `nil`,
  /// passing the unwrapped value as a parameter.
  ///
  /// Use the `map` method with a closure that returns a non-optional value.
  /// This example performs an arithmetic operation on an
  /// optional integer.
  ///
  ///     let possibleNumber = Int("42")
  ///     let possibleSquare = possibleNumber.map { $0 * $0 }
  ///     print(possibleSquare)
  ///     // Prints "Optional(1764)"
  ///
  ///     let noNumber = Int("abc")
  ///     let noSquare = noNumber.map { $0 * $0 }
  ///     print(noSquare)
  ///     // Prints "nil"
  ///
  /// - Parameter transform: A closure that takes the unwrapped value
  ///   of the instance.
  /// - Returns: The result of the given closure. If this instance is `nil`,
  ///   returns `nil`.
  @inlinable
  public func map<U>(_ transform: (Wrapped) throws -> U) rethrows -> U? {
    switch self {
    case .some(let wrapped):
      return .some(try transform(wrapped))
    case .none:
      return .none
    }
  }

  /// Evaluates the given closure when this `Optional` instance is not `nil`,
  /// passing the unwrapped value as a parameter.
  ///
  /// Use the `flatMap` method with a closure that returns an optional value.
  /// This example performs a failing dictionary lookup on an
  /// optional string, only proceeding if the initial value is not `nil`:
  ///
  ///     let possibleNumber = Int("42")
  ///     let nonOverflowingSquare = possibleNumber.flatMap { x -> Int? in
  ///         let (result, overflowed) = x.multipliedReportingOverflow(by: x)
  ///         return overflowed ? nil : result
  ///     }
  ///     print(nonOverflowingSquare)
  ///     // Prints "Optional(1764)"
  ///
  /// - Parameter transform: A closure that takes the unwrapped value
  ///   of the instance.
  /// - Returns: The result of the given closure. If this instance is `nil`,
  ///   returns `nil`.
  @inlinable
  public func flatMap<U>(_ transform: (Wrapped) throws -> U?) rethrows -> U? {
    switch self {
    case .some(let wrapped):
      return try transform(wrapped)
    case .none:
      return .none
    }
  }

  /// Returns the wrapped value if not `nil`; otherwise, returns the provided default value.
  ///
  /// - Parameter defaultValue: The default value to use when this instance is `nil`.
  /// - Returns: The wrapped value of this instance or the provided default value.
  @inlinable
  public func ?? (optional: Wrapped?, defaultValue: @autoclosure () throws -> Wrapped) rethrows -> Wrapped {
    switch optional {
    case .some(let wrapped):
      return wrapped
    case .none:
      return try defaultValue()
    }
  }

  /// Returns the wrapped value if not `nil`; otherwise, returns the provided default optional value.
  ///
  /// - Parameter defaultValue: The default optional value to use when this instance is `nil`.
  /// - Returns: This instance if not `nil`; otherwise, the provided default value.
  @inlinable
  public func ?? (optional: Wrapped?, defaultValue: @autoclosure () throws -> Wrapped?) rethrows -> Wrapped? {
    switch optional {
    case .some:
      return optional
    case .none:
      return try defaultValue()
    }
  }
}

// MARK: - Equatable conformance

extension Optional: Equatable where Wrapped: Equatable {
  /// Returns a Boolean value indicating whether two optional instances are equal.
  ///
  /// Use this operator to compare any two optional instances of a type that
  /// conforms to the `Equatable` protocol. The comparison returns `true` if
  /// both arguments are `nil` or if both arguments wrap values that are equal.
  /// Conversely, the comparison returns `false` if only one of the arguments
  /// is `nil` or if the two arguments wrap unequal values.
  ///
  ///     let group1 = [1, 2, 3, 4, 5]
  ///     let group2 = [1, 3, 5, 7, 9]
  ///     if group1.first == group2.first {
  ///         print("The two groups start the same.")
  ///     }
  ///     // Prints "The two groups start the same."
  ///
  /// You can also use this operator to compare a non-optional value to an
  /// optional that wraps the same type. The non-optional value is wrapped as
  /// an optional before the comparison is made. In this example, the `Int`
  /// value `5` is wrapped as an optional before comparing to the optional `x`:
  ///
  ///     let x: Int? = 5
  ///     if x == 5 {
  ///         print("x is five")
  ///     }
  ///     // Prints "x is five"
  ///
  /// - Parameters:
  ///   - lhs: An optional value to compare.
  ///   - rhs: Another optional value to compare.
  @inlinable
  public static func == (lhs: Wrapped?, rhs: Wrapped?) -> Bool {
    switch (lhs, rhs) {
    case (.none, .none): return true
    case (.some(let lhsValue), .some(let rhsValue)): return lhsValue == rhsValue
    default: return false
    }
  }

  /// Returns a Boolean value indicating whether the optional and non-optional arguments are equal.
  @inlinable
  public static func == (lhs: Wrapped?, rhs: Wrapped) -> Bool {
    return lhs == .some(rhs)
  }

  /// Returns a Boolean value indicating whether the non-optional and optional arguments are equal.
  @inlinable
  public static func == (lhs: Wrapped, rhs: Wrapped?) -> Bool {
    return .some(lhs) == rhs
  }

  /// Returns a Boolean value indicating whether two optional instances are not equal.
  @inlinable
  public static func != (lhs: Wrapped?, rhs: Wrapped?) -> Bool {
    return !(lhs == rhs)
  }
}

// MARK: - Comparable conformance

extension Optional: Comparable where Wrapped: Comparable {
  /// Returns a Boolean value indicating whether the first argument is ordered
  /// before the second.
  ///
  /// A `nil` value is ordered before any non-`nil` value.
  @inlinable
  public static func < (lhs: Wrapped?, rhs: Wrapped?) -> Bool {
    switch (lhs, rhs) {
    case (.none, .some): return true
    case (.some(let lhsValue), .some(let rhsValue)): return lhsValue < rhsValue
    default: return false
    }
  }
}

// MARK: - Hashable conformance

extension Optional: Hashable where Wrapped: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    switch self {
    case .none:
      hasher.combine(0 as UInt8)
    case .some(let wrapped):
      hasher.combine(1 as UInt8)
      hasher.combine(wrapped)
    }
  }
}

// MARK: - CustomStringConvertible

extension Optional: CustomStringConvertible {
  /// A textual representation of this instance.
  ///
  /// If this instance is `nil`, this property returns `"nil"`. Otherwise,
  /// this property returns `"Optional(\(Wrapped))"`.
  public var description: String {
    switch self {
    case .some(let wrapped):
      return "Optional(\(wrapped))"
    case .none:
      return "nil"
    }
  }
}

// MARK: - CustomDebugStringConvertible

extension Optional: CustomDebugStringConvertible {
  /// A textual representation of this instance, suitable for debugging.
  public var debugDescription: String {
    switch self {
    case .some(let wrapped):
      var result = "Optional("
      debugPrint(wrapped, terminator: "", to: &result)
      result += ")"
      return result
    case .none:
      return "nil"
    }
  }
}

// MARK: - Forced unwrapping

extension Optional {
  /// The wrapped value, if the instance is not `nil`.
  ///
  /// If the instance is `nil`, accessing this property triggers a runtime error.
  /// When the possibility of a `nil` value is expected, test for `nil` before
  /// accessing this property or use conditional unwrapping instead.
  ///
  ///     let number = Int("42")!
  ///     print(number)
  ///     // Prints "42"
  ///
  ///     let notANumber = Int("abc")!
  ///     // Runtime error
  @inlinable
  public var unsafelyUnwrapped: Wrapped {
    if let x = self {
      return x
    }
    _debugPreconditionFailure("unsafelyUnwrapped of nil optional")
  }
}

// MARK: - Pattern matching support

/// Performs a nil-coalescing operation, returning the wrapped value of an
/// `Optional` instance or a default value.
///
/// A nil-coalescing operation unwraps the left operand if it has a value, or
/// it returns the right operand as a default. The result of this operation
/// will have the non-optional type of the left operand's `Wrapped` type.
///
/// This operator uses short-circuit evaluation: `optional` is checked first,
/// and `defaultValue` is evaluated only if `optional` is `nil`. For example:
///
///     func getDefault() -> Int {
///         print("Calculating default...")
///         return 42
///     }
///
///     let goodNumber = Int("100")
///     let number = goodNumber ?? getDefault()
///     // goodNumber != nil, so number == 100
///
///     let notSoGoodNumber = Int("invalid-input")
///     let number2 = notSoGoodNumber ?? getDefault()
///     // Prints "Calculating default..."
///     // notSoGoodNumber == nil, so number2 == 42
///
/// In this example, `getDefault()` is never called when `goodNumber` contains
/// a value.
///
/// - Parameters:
///   - optional: An optional value.
///   - defaultValue: A value to use as a default. `defaultValue` is the same
///     type as the `Wrapped` type of `optional`.
@_transparent
public func ?? <T>(optional: T?, defaultValue: @autoclosure () throws -> T)
  rethrows -> T {
  return try optional ?? defaultValue()
}

/// Performs a nil-coalescing operation, returning the wrapped value of an
/// `Optional` instance or a default `Optional` value.
///
/// A nil-coalescing operation unwraps the left operand if it has a value, or
/// returns the right operand as a default. The result of this operation
/// will be the same type as its arguments.
///
/// This operator uses short-circuit evaluation: `optional` is checked first,
/// and `defaultValue` is evaluated only if `optional` is `nil`. For example:
///
///     let goodNumber = Int("100")
///     let number = goodNumber ?? Int("42")
///     // goodNumber != nil, so number == Optional(100)
///
///     let notSoGoodNumber = Int("invalid-input")
///     let number2 = notSoGoodNumber ?? Int("42")
///     // notSoGoodNumber == nil, so number2 == Optional(42)
///
/// In this example, the right side of the `??` operator is only evaluated
/// if the left side is `nil`.
///
/// - Parameters:
///   - optional: An optional value.
///   - defaultValue: A value to use as a default. `defaultValue` and
///     `optional` have the same type.
@_transparent
public func ?? <T>(optional: T?, defaultValue: @autoclosure () throws -> T?)
  rethrows -> T? {
  return try optional ?? defaultValue()
}