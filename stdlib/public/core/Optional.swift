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

/// A type that can represent either a wrapped value or `nil`, the absence of a
/// value.
///
/// You use the `Optional` type whenever you use optional values, even if you
/// never type the word `Optional`. Swift's type system usually shows the
/// wrapped type's name with a trailing question mark (`?`) instead of showing
/// the full type name. For example, if a variable has the type `Int?`, that's
/// just another way of writing `Optional<Int>`. The shortened form is
/// preferred for ease of reading and writing code.
///
/// The types of `shortForm` and `longForm` in the following code sample are
/// the same:
///
///     let shortForm: Int? = Int("42")
///     let longForm: Optional<Int> = Int("42")
///
/// The `Optional` type is an enumeration with two cases. `Optional.none` is
/// equivalent to the `nil` literal. `Optional.some(Wrapped)` stores a wrapped
/// value. For example:
///
///     let number: Int? = Optional.some(42)
///     let noNumber: Int? = Optional.none
///     print(noNumber == nil)
///     // Prints "true"
///
/// You must unwrap the value of an `Optional` instance before you can use it
/// in many contexts. Because Swift provides several ways to safely unwrap
/// optional values, you can choose the one that helps you write clear,
/// concise code.
///
/// The following examples use this dictionary of image names and file paths:
///
///     let imagePaths = ["star": "/glyphs/star.png",
///                       "portrait": "/images/content/portrait.jpg",
///                       "spacer": "/images/shared/spacer.gif"]
///
/// Getting a dictionary's value using a key returns an optional value, so
/// `imagePaths["star"]` has type `Optional<String>` or, written in the
/// preferred manner, `String?`.
///
/// Optional Binding
/// ----------------
///
/// To conditionally bind the wrapped value of an `Optional` instance to a new
/// variable, use one of the optional binding control structures, including
/// `if let`, `guard let`, and `switch`.
///
///     if let starPath = imagePaths["star"] {
///         print("The star image is at '\(starPath)'")
///     } else {
///         print("Couldn't find the star image")
///     }
///     // Prints "The star image is at '/glyphs/star.png'"
///
/// Optional Chaining
/// -----------------
///
/// To safely access the properties and methods of a wrapped instance, use the
/// postfix optional chaining operator (`?`). The following example uses
/// optional chaining to access the `hasSuffix(_:)` method on a `String?`
/// instance.
///
///     if let isPNG = imagePaths["star"]?.hasSuffix(".png") {
///         print("The star image is in PNG format")
///     }
///     // Prints "The star image is in PNG format"
///
/// Using the Nil-Coalescing Operator
/// ---------------------------------
///
/// Use the nil-coalescing operator (`??`) to supply a default value in case
/// the `Optional` instance is `nil`. Here a default path is supplied for an
/// image that is missing from `imagePaths`.
///
///     let defaultImagePath = "/images/default.png"
///     let heartPath = imagePaths["heart"] ?? defaultImagePath
///     print(heartPath)
///     // Prints "/images/default.png"
///
/// The `??` operator also works with another `Optional` instance on the
/// right-hand side. As a result, you can chain multiple `??` operators
/// together.
///
///     let shapePath = imagePaths["cir"] ?? imagePaths["squ"] ?? defaultImagePath
///     print(shapePath)
///     // Prints "/images/default.png"
///
/// Unconditional Unwrapping
/// ------------------------
///
/// When you're certain that an instance of `Optional` contains a value, you
/// can unconditionally unwrap the value by using the forced
/// unwrap operator (postfix `!`). For example, the result of the failable `Int`
/// initializer is unconditionally unwrapped in the example below.
///
///     let number = Int("42")!
///     print(number)
///     // Prints "42"
///
/// You can also perform unconditional optional chaining by using the postfix
/// `!` operator.
///
///     let isPNG = imagePaths["star"]!.hasSuffix(".png")
///     print(isPNG)
///     // Prints "true"
///
/// Unconditionally unwrapping a `nil` instance with `!` triggers a runtime
/// error.
@_fixed_layout
public enum Optional<Wrapped> : ExpressibleByNilLiteral {
  // The compiler has special knowledge of Optional<Wrapped>, including the fact
  // that it is an `enum` with cases named `none` and `some`.

  /// The absence of a value.
  ///
  /// In code, the absence of a value is typically written using the `nil`
  /// literal rather than the explicit `.none` enumeration case.
  case none

  /// The presence of a value, stored as `Wrapped`.
  case some(Wrapped)

  /// Creates an instance that stores the given value.
  @_transparent
  public init(_ some: Wrapped) { self = .some(some) }

  /// Evaluates the given closure when this `Optional` instance is not `nil`,
  /// passing the unwrapped value as a parameter.
  ///
  /// Use the `map` method with a closure that returns a nonoptional value.
  /// This example performs an arithmetic operation on an
  /// optional integer.
  ///
  ///     let possibleNumber: Int? = Int("42")
  ///     let possibleSquare = possibleNumber.map { $0 * $0 }
  ///     print(possibleSquare)
  ///     // Prints "Optional(1746)"
  ///
  ///     let noNumber: Int? = nil
  ///     let noSquare = noNumber.map { $0 * $0 }
  ///     print(noSquare)
  ///     // Prints "nil"
  ///
  /// - Parameter transform: A closure that takes the unwrapped value
  ///   of the instance.
  /// - Returns: The result of the given closure. If this instance is `nil`,
  ///   returns `nil`.
  public func map<U>(
    _ transform: (Wrapped) throws -> U
  ) rethrows -> U? {
    switch self {
    case .some(let y):
      return .some(try transform(y))
    case .none:
      return .none
    }
  }

  /// Evaluates the given closure when this `Optional` instance is not `nil`,
  /// passing the unwrapped value as a parameter.
  ///
  /// Use the `flatMap` method with a closure that returns an optional value.
  /// This example performs an arithmetic operation with an optional result on
  /// an optional integer.
  ///
  ///     let possibleNumber: Int? = Int("42")
  ///     let nonOverflowingSquare = possibleNumber.flatMap { x -> Int? in
  ///         let (result, overflowed) = Int.multiplyWithOverflow(x, x)
  ///         return overflowed ? nil : result
  ///     }
  ///     print(nonOverflowingSquare)
  ///     // Prints "Optional(1746)"
  ///
  /// - Parameter transform: A closure that takes the unwrapped value
  ///   of the instance.  
  /// - Returns: The result of the given closure. If this instance is `nil`,
  ///   returns `nil`.
  public func flatMap<U>(
    _ transform: (Wrapped) throws -> U?
  ) rethrows -> U? {
    switch self {
    case .some(let y):
      return try transform(y)
    case .none:
      return .none
    }
  }

  /// Creates an instance initialized with `nil`.
  ///
  /// Do not call this initializer directly. It is used by the compiler when you
  /// initialize an `Optional` instance with a `nil` literal. For example:
  ///
  ///     var i: Index? = nil
  ///
  /// In this example, the assignment to the `i` variable calls this
  /// initializer behind the scenes.
  @_transparent
  public init(nilLiteral: ()) {
    self = .none
  }

  /// The wrapped value of this instance, unwrapped without checking whether
  /// the instance is `nil`.
  ///
  /// The `unsafelyUnwrapped` property provides the same value as the forced
  /// unwrap operator (postfix `!`). However, in optimized builds (`-O`), no
  /// check is performed to ensure that the current instance actually has a
  /// value. Accessing this property in the case of a `nil` value is a serious
  /// programming error and could lead to undefined behavior or a runtime
  /// error.
  ///
  /// In debug builds (`-Onone`), the `unsafelyUnwrapped` property has the same
  /// behavior as using the postfix `!` operator and triggers a runtime error
  /// if the instance is `nil`.
  ///
  /// The `unsafelyUnwrapped` property is recommended over calling the
  /// `unsafeBitCast(_:)` function because the property is more restrictive
  /// and because accessing the property still performs checking in debug
  /// builds.
  ///
  /// - Warning: This property trades safety for performance.  Use
  ///   `unsafelyUnwrapped` only when you are confident that this instance
  ///   will never be equal to `nil` and only after you've tried using the
  ///   postfix `!` operator.
  public var unsafelyUnwrapped: Wrapped {
    @inline(__always)
    get {
      if let x = self {
        return x
      }
      _debugPreconditionFailure("unsafelyUnwrapped of nil optional")
    }
  }

  /// - Returns: `unsafelyUnwrapped`.
  ///
  /// This version is for internal stdlib use; it avoids any checking
  /// overhead for users, even in Debug builds.
  public // SPI(SwiftExperimental)
  var _unsafelyUnwrappedUnchecked: Wrapped {
    @inline(__always)
    get {
      if let x = self {
        return x
      }
      _sanityCheckFailure("_unsafelyUnwrappedUnchecked of nil optional")
    }
  }
}

extension Optional : CustomDebugStringConvertible {
  /// A textual representation of this instance, suitable for debugging.
  public var debugDescription: String {
    switch self {
    case .some(let value):
      var result = "Optional("
      debugPrint(value, terminator: "", to: &result)
      result += ")"
      return result
    case .none:
      return "nil"
    }
  }
}

extension Optional : CustomReflectable {
  public var customMirror: Mirror {
    switch self {
    case .some(let value):
      return Mirror(
        self,
        children: [ "some": value ],
        displayStyle: .optional)
    case .none:
      return Mirror(self, children: [:], displayStyle: .optional)
    }
  }
}

@_transparent
public // COMPILER_INTRINSIC
func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word) {
  _preconditionFailure(
    "unexpectedly found nil while unwrapping an Optional value",
    file: StaticString(_start: _filenameStart,
                       utf8CodeUnitCount: _filenameLength,
                       isASCII: _filenameIsASCII),
    line: UInt(_line))
}

public func == <T: Equatable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l == r
  case (nil, nil):
    return true
  default:
    return false
  }
}

public func != <T : Equatable>(lhs: T?, rhs: T?) -> Bool {
  return !(lhs == rhs)
}

// Enable pattern matching against the nil literal, even if the element type
// isn't equatable.
@_fixed_layout
public struct _OptionalNilComparisonType : ExpressibleByNilLiteral {
  /// Create an instance initialized with `nil`.
  @_transparent
  public init(nilLiteral: ()) {
  }
}
@_transparent
public func ~= <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .some(_):
    return false
  case .none:
    return true
  }
}

// Enable equality comparisons against the nil literal, even if the
// element type isn't equatable
@_transparent
public func == <T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool {
  switch lhs {
  case .some(_):
    return false
  case .none:
    return true
  }
}

@_transparent
public func != <T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool {
  switch lhs {
  case .some(_):
    return true
  case .none:
    return false
  }
}

@_transparent
public func == <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .some(_):
    return false
  case .none:
    return true
  }
}

@_transparent
public func != <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .some(_):
    return true
  case .none:
    return false
  }
}

/// Performs a nil-coalescing operation, returning the wrapped value of an
/// `Optional` instance or a default value.
///
/// A nil-coalescing operation unwraps the left-hand side if it has a value, or
/// it returns the right-hand side as a default. The result of this operation
/// will have the nonoptional type of the left-hand side's `Wrapped` type.
///
/// This operator uses short-circuit evaluation: `optional` is checked first,
/// and `defaultValue` is evaluated only if `optional` is `nil`. For example:
///
///     func getDefault() -> Int {
///         print("Calculating default...")
///         return 42
///     }
///
///     let goodNumber = Int("100") ?? getDefault()
///     // goodNumber == 100
///
///     let notSoGoodNumber = Int("invalid-input") ?? getDefault()
///     // Prints "Calculating default..."
///     // notSoGoodNumber == 42
///
/// In this example, `goodNumber` is assigned a value of `100` because
/// `Int("100")` succeeded in returning a non-`nil` result. When
/// `notSoGoodNumber` is initialized, `Int("invalid-input")` fails and returns
/// `nil`, and so the `getDefault()` method is called to supply a default
/// value.
///
/// - Parameters:
///   - optional: An optional value.
///   - defaultValue: A value to use as a default. `defaultValue` is the same
///     type as the `Wrapped` type of `optional`.
@_transparent
public func ?? <T>(optional: T?, defaultValue: @autoclosure () throws -> T)
    rethrows -> T {
  switch optional {
  case .some(let value):
    return value
  case .none:
    return try defaultValue()
  }
}

/// Performs a nil-coalescing operation, returning the wrapped value of an
/// `Optional` instance or a default `Optional` value.
///
/// A nil-coalescing operation unwraps the left-hand side if it has a value, or
/// returns the right-hand side as a default. The result of this operation
/// will be the same type as its arguments.
///
/// This operator uses short-circuit evaluation: `optional` is checked first,
/// and `defaultValue` is evaluated only if `optional` is `nil`. For example:
///
///     let goodNumber = Int("100") ?? Int("42")
///     print(goodNumber)
///     // Prints "Optional(100)"
///
///     let notSoGoodNumber = Int("invalid-input") ?? Int("42")
///     print(notSoGoodNumber)
///     // Prints "Optional(42)"
///
/// In this example, `goodNumber` is assigned a value of `100` because
/// `Int("100")` succeeds in returning a non-`nil` result. When
/// `notSoGoodNumber` is initialized, `Int("invalid-input")` fails and returns
/// `nil`, and so `Int("42")` is called to supply a default value.
///
/// Because the result of this nil-coalescing operation is itself an optional
/// value, you can chain default values by using `??` multiple times. The
/// first optional value that isn't `nil` stops the chain and becomes the
/// result of the whole expression. The next example tries to find the correct
/// text for a greeting in two separate dictionaries before falling back to a
/// static default.
///
///     let greeting = userPrefs[greetingKey] ??
///         defaults[greetingKey] ?? "Greetings!"
///
/// If `userPrefs[greetingKey]` has a value, that value is assigned to
/// `greeting`. If not, any value in `defaults[greetingKey]` will succeed, and
/// if not that, `greeting` will be set to the non-optional default value,
/// `"Greetings!"`.
///
/// - Parameters:
///   - optional: An optional value.
///   - defaultValue: A value to use as a default. `defaultValue` and
///     `optional` have the same type.
@_transparent
public func ?? <T>(optional: T?, defaultValue: @autoclosure () throws -> T?)
    rethrows -> T? {
  switch optional {
  case .some(let value):
    return value
  case .none:
    return try defaultValue()
  }
}

extension Optional {
  @available(*, unavailable, renamed: "none")
  public static var None: Optional<Wrapped> {
    return .none
  }
  @available(*, unavailable, renamed: "some")
  public static func Some(_ x: Wrapped) -> Optional<Wrapped> {
    return .some(x)
  }

}
