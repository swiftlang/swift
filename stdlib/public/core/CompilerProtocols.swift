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
// Intrinsic protocols shared with the compiler
//===----------------------------------------------------------------------===//

/// A type that can be converted to and from an associated raw value.
///
/// With a `RawRepresentable` type, you can switch back and forth between a
/// custom type and an associated `RawValue` type without losing the value of
/// the original `RawRepresentable` type. Using the raw value of a conforming
/// type streamlines interoperation with Objective-C and legacy APIs and
/// simplifies conformance to other protocols, such as `Equatable`,
/// `Comparable`, and `Hashable`.
///
/// The `RawRepresentable` protocol is seen mainly in two categories of types:
/// enumerations with raw value types and option sets.
///
/// Enumerations with Raw Values
/// ============================
///
/// For any enumeration with a string, integer, or floating-point raw type, the
/// Swift compiler automatically adds `RawRepresentable` conformance. When
/// defining your own custom enumeration, you give it a raw type by specifying
/// the raw type as the first item in the enumeration's type inheritance list.
/// You can also use literals to specify values for one or more cases.
///
/// For example, the `Counter` enumeration defined here has an `Int` raw value
/// type and gives the first case a raw value of `1`:
///
///     enum Counter: Int {
///         case one = 1, two, three, four, five
///     }
///
/// You can create a `Counter` instance from an integer value between 1 and 5
/// by using the `init?(rawValue:)` initializer declared in the
/// `RawRepresentable` protocol. This initializer is failable because although
/// every case of the `Counter` type has a corresponding `Int` value, there
/// are many `Int` values that *don't* correspond to a case of `Counter`.
///
///     for i in 3...6 {
///         print(Counter(rawValue: i))
///     }
///     // Prints "Optional(Counter.three)"
///     // Prints "Optional(Counter.four)"
///     // Prints "Optional(Counter.five)"
///     // Prints "nil"
///
/// Option Sets
/// ===========
///
/// Option sets all conform to `RawRepresentable` by inheritance using the
/// `OptionSet` protocol. Whether using an option set or creating your own,
/// you use the raw value of an option set instance to store the instance's
/// bitfield. The raw value must therefore be of a type that conforms to the
/// `BitwiseOperations` protocol, such as `UInt8` or `Int`. For example, the
/// `Direction` type defines an option set for the four directions you can
/// move in a game.
///
///     struct Directions: OptionSet {
///         let rawValue: UInt8
///
///         static let up    = Directions(rawValue: 1 << 0)
///         static let down  = Directions(rawValue: 1 << 1)
///         static let left  = Directions(rawValue: 1 << 2)
///         static let right = Directions(rawValue: 1 << 3)
///     }
///
/// Unlike enumerations, option sets provide a nonfailable `init(rawValue:)`
/// initializer to convert from a raw value, because option sets don't have an
/// enumerated list of all possible cases. Option set values have
/// a one-to-one correspondence with their associated raw values.
///
/// In the case of the `Directions` option set, an instance can contain zero,
/// one, or more of the four defined directions. This example declares a
/// constant with three currently allowed moves. The raw value of the
/// `allowedMoves` instance is the result of the bitwise OR of its three
/// members' raw values:
///
///     let allowedMoves: Directions = [.up, .down, .left]
///     print(allowedMoves.rawValue)
///     // Prints "7"
///
/// Option sets use bitwise operations on their associated raw values to
/// implement their mathematical set operations. For example, the `contains()`
/// method on `allowedMoves` performs a bitwise AND operation to check whether
/// the option set contains an element.
///
///     print(allowedMoves.contains(.right))
///     // Prints "false"
///     print(allowedMoves.rawValue & Directions.right.rawValue)
///     // Prints "0"
///
/// - SeeAlso: `OptionSet`, `BitwiseOperations`
public protocol RawRepresentable {
  /// The raw type that can be used to represent all values of the conforming
  /// type.
  ///
  /// Every distinct value of the conforming type has a corresponding unique
  /// value of the `RawValue` type, but there may be values of the `RawValue`
  /// type that don't have a corresponding value of the conforming type.
  associatedtype RawValue

  /// Creates a new instance with the specified raw value.
  ///
  /// If there is no value of the type that corresponds with the specified raw
  /// value, this initializer returns `nil`. For example:
  ///
  ///     enum PaperSize: String {
  ///         case A4, A5, Letter, Legal
  ///     }
  ///
  ///     print(PaperSize(rawValue: "Legal"))
  ///     // Prints "Optional("PaperSize.Legal")"
  ///
  ///     print(PaperSize(rawValue: "Tabloid"))
  ///     // Prints "nil"
  ///
  /// - Parameter rawValue: The raw value to use for the new instance.
  init?(rawValue: RawValue)

  /// The corresponding value of the raw type.
  ///
  /// A new instance initialized with `rawValue` will be equivalent to this
  /// instance. For example:
  ///
  ///     enum PaperSize: String {
  ///         case A4, A5, Letter, Legal
  ///     }
  ///
  ///     let selectedSize = PaperSize.Letter
  ///     print(selectedSize.rawValue)
  ///     // Prints "Letter"
  ///
  ///     print(selectedSize == PaperSize(rawValue: selectedSize.rawValue)!)
  ///     // Prints "true"
  var rawValue: RawValue { get }
}

/// Returns a Boolean value indicating whether the two arguments are equal.
///
/// - Parameters:
///   - lhs: A raw-representable instance.
///   - rhs: A second raw-representable instance.
public func == <T : RawRepresentable>(lhs: T, rhs: T) -> Bool
  where T.RawValue : Equatable {
  return lhs.rawValue == rhs.rawValue
}

/// Returns a Boolean value indicating whether the two arguments are not equal.
///
/// - Parameters:
///   - lhs: A raw-representable instance.
///   - rhs: A second raw-representable instance.
public func != <T : RawRepresentable>(lhs: T, rhs: T) -> Bool
  where T.RawValue : Equatable {
  return lhs.rawValue != rhs.rawValue
}

// This overload is needed for ambiguity resolution against the
// implementation of != for T : Equatable
/// Returns a Boolean value indicating whether the two arguments are not equal.
///
/// - Parameters:
///   - lhs: A raw-representable instance.
///   - rhs: A second raw-representable instance.
public func != <T : Equatable>(lhs: T, rhs: T) -> Bool
  where T : RawRepresentable, T.RawValue : Equatable {
  return lhs.rawValue != rhs.rawValue
}

/// A type that can be initialized using the nil literal, `nil`.
///
/// `nil` has a specific meaning in Swift---the absence of a value. Only the
/// `Optional` type conforms to `ExpressibleByNilLiteral`.
/// `ExpressibleByNilLiteral` conformance for types that use `nil` for other
/// purposes is discouraged.
///
/// - SeeAlso: `Optional`
public protocol ExpressibleByNilLiteral {
  /// Creates an instance initialized with `nil`.
  init(nilLiteral: ())
}

public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

/// A type that can be initialized with an integer literal.
///
/// The standard library integer and floating-point types, such as `Int` and
/// `Double`, conform to the `ExpressibleByIntegerLiteral` protocol. You can
/// initialize a variable or constant of any of these types by assigning an
/// integer literal.
///
///     // Type inferred as 'Int'
///     let cookieCount = 12
///
///     // An array of 'Int'
///     let chipsPerCookie = [21, 22, 25, 23, 24, 19]
///
///     // A floating-point value initialized using an integer literal
///     let redPercentage: Double = 1
///     // redPercentage == 1.0
///
/// Conforming to ExpressibleByIntegerLiteral
/// =========================================
///
/// To add `ExpressibleByIntegerLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByIntegerLiteral {
  /// A type that represents an integer literal.
  ///
  /// The standard library integer and floating-point types are all valid types
  /// for `IntegerLiteralType`.
  associatedtype IntegerLiteralType : _ExpressibleByBuiltinIntegerLiteral

  /// Creates an instance initialized to the specified integer value.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using an integer literal. For example:
  ///
  ///     let x = 23
  ///
  /// In this example, the assignment to the `x` constant calls this integer
  /// literal initializer behind the scenes.
  ///
  /// - Parameter value: The value to create.
  init(integerLiteral value: IntegerLiteralType)
}

public protocol _ExpressibleByBuiltinFloatLiteral {
  init(_builtinFloatLiteral value: _MaxBuiltinFloatType)
}

/// A type that can be initialized with a floating-point literal.
///
/// The standard library floating-point types---`Float`, `Double`, and
/// `Float80` where available---all conform to the `ExpressibleByFloatLiteral`
/// protocol. You can initialize a variable or constant of any of these types
/// by assigning a floating-point literal.
///
///     // Type inferred as 'Double'
///     let threshold = 6.0
///
///     // An array of 'Double'
///     let measurements = [2.2, 4.1, 3.65, 4.2, 9.1]
///
/// Conforming to ExpressibleByFloatLiteral
/// =======================================
///
/// To add `ExpressibleByFloatLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByFloatLiteral {
  /// A type that represents a floating-point literal.
  ///
  /// Valid types for `FloatLiteralType` are `Float`, `Double`, and `Float80`
  /// where available.
  associatedtype FloatLiteralType : _ExpressibleByBuiltinFloatLiteral
  
  /// Creates an instance initialized to the specified floating-point value.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using a floating-point literal. For example:
  ///
  ///     let x = 21.5
  ///
  /// In this example, the assignment to the `x` constant calls this
  /// floating-point literal initializer behind the scenes.
  ///
  /// - Parameter value: The value to create.
  init(floatLiteral value: FloatLiteralType)
}

public protocol _ExpressibleByBuiltinBooleanLiteral {
  init(_builtinBooleanLiteral value: Builtin.Int1)
}

/// A type that can be initialized with the Boolean literals `true` and
/// `false`.
///
/// Only three types provided by Swift---`Bool`, `DarwinBoolean`, and
/// `ObjCBool`---are treated as Boolean values. Expanding this set to include
/// types that represent more than simple Boolean values is discouraged.
///
/// To add `ExpressibleByBooleanLiteral` conformance to your custom type,
/// implement the `init(booleanLiteral:)` initializer that creates an instance
/// of your type with the given Boolean value.
public protocol ExpressibleByBooleanLiteral {
  /// A type that represents a Boolean literal, such as `Bool`.
  associatedtype BooleanLiteralType : _ExpressibleByBuiltinBooleanLiteral

  /// Creates an instance initialized to the given Boolean value.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using one of the Boolean literals `true` and `false`. For
  /// example:
  ///
  ///     let twasBrillig = true
  ///
  /// In this example, the assignment to the `twasBrillig` constant calls this
  /// Boolean literal initializer behind the scenes.
  ///
  /// - Parameter value: The value of the new instance.
  init(booleanLiteral value: BooleanLiteralType)
}

public protocol _ExpressibleByBuiltinUnicodeScalarLiteral {
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
}

/// A type that can be initialized with a string literal containing a single
/// Unicode scalar value.
///
/// The `String`, `StaticString`, `Character`, and `UnicodeScalar` types all
/// conform to the `ExpressibleByUnicodeScalarLiteral` protocol. You can
/// initialize a variable of any of these types using a string literal that
/// holds a single Unicode scalar.
///
///     let ñ: UnicodeScalar = "ñ"
///     print(ñ)
///     // Prints "ñ"
///
/// Conforming to ExpressibleByUnicodeScalarLiteral
/// ===============================================
///
/// To add `ExpressibleByUnicodeScalarLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByUnicodeScalarLiteral {
  /// A type that represents a Unicode scalar literal.
  ///
  /// Valid types for `UnicodeScalarLiteralType` are `UnicodeScalar`,
  /// `String`, and `StaticString`.
  associatedtype UnicodeScalarLiteralType : _ExpressibleByBuiltinUnicodeScalarLiteral

  /// Creates an instance initialized to the given value.
  ///
  /// - Parameter value: The value of the new instance.
  init(unicodeScalarLiteral value: UnicodeScalarLiteralType)
}

public protocol _ExpressibleByBuiltinExtendedGraphemeClusterLiteral
  : _ExpressibleByBuiltinUnicodeScalarLiteral {

  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1)
}

/// A type that can be initialized with a string literal containing a single
/// extended grapheme cluster.
///
/// An *extended grapheme cluster* is a group of one or more Unicode code
/// points that approximates a single user-perceived character.  Many
/// individual characters, such as "é", "김", and "🇮🇳", can be made up of
/// multiple Unicode code points. These code points are combined by Unicode's
/// boundary algorithms into extended grapheme clusters.
///
/// The `String`, `StaticString`, and `Character` types conform to the
/// `ExpressibleByExtendedGraphemeClusterLiteral` protocol. You can initialize a
/// variable or constant of any of these types using a string literal that
/// holds a single character.
///
///     let snowflake: Character = "❄︎"
///     print(snowflake)
///     // Prints "❄︎"
///
/// Conforming to ExpressibleByExtendedGraphemeClusterLiteral
/// =========================================================
///
/// To add `ExpressibleByExtendedGraphemeClusterLiteral` conformance to your
/// custom type, implement the required initializer.
public protocol ExpressibleByExtendedGraphemeClusterLiteral
  : ExpressibleByUnicodeScalarLiteral {

  /// A type that represents an extended grapheme cluster literal.
  ///
  /// Valid types for `ExtendedGraphemeClusterLiteralType` are `Character`,
  /// `String`, and `StaticString`.
  associatedtype ExtendedGraphemeClusterLiteralType
    : _ExpressibleByBuiltinExtendedGraphemeClusterLiteral
  
  /// Creates an instance initialized to the given value.
  ///
  /// - Parameter value: The value of the new instance.
  init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType)
}

public protocol _ExpressibleByBuiltinStringLiteral
  : _ExpressibleByBuiltinExtendedGraphemeClusterLiteral {

  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1)
}

public protocol _ExpressibleByBuiltinUTF16StringLiteral
  : _ExpressibleByBuiltinStringLiteral {

  init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word)
}

/// A type that can be initialized with a string literal.
///
/// The `String` and `StaticString` types conform to the
/// `ExpressibleByStringLiteral` protocol. You can initialize a variable or
/// constant of either of these types using a string literal of any length.
///
///     let picnicGuest = "Deserving porcupine"
///
/// Conforming to ExpressibleByStringLiteral
/// ========================================
///
/// To add `ExpressibleByStringLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByStringLiteral
  : ExpressibleByExtendedGraphemeClusterLiteral {
  // FIXME: when we have default function implementations in protocols, provide
  // an implementation of init(extendedGraphemeClusterLiteral:).
  
  /// A type that represents a string literal.
  ///
  /// Valid types for `StringLiteralType` are `String` and `StaticString`.
  associatedtype StringLiteralType : _ExpressibleByBuiltinStringLiteral
  
  /// Creates an instance initialized to the given string value.
  ///
  /// - Parameter value: The value of the new instance.
  init(stringLiteral value: StringLiteralType)
}

/// A type that can be initialized using an array literal.
///
/// An array literal is a simple way of expressing a list of values. Simply
/// surround a comma-separated list of values, instances, or literals with
/// square brackets to create an array literal. You can use an array literal
/// anywhere an instance of an `ExpressibleByArrayLiteral` type is expected: as
/// a value assigned to a variable or constant, as a parameter to a method or
/// initializer, or even as the subject of a nonmutating operation like
/// `map(_:)` or `filter(_:)`.
///
/// Arrays, sets, and option sets all conform to `ExpressibleByArrayLiteral`, 
/// and your own custom types can as well. Here's an example of creating a set 
/// and an array using array literals:
///
///     let employeesSet: Set<String> = ["Amir", "Jihye", "Dave", "Alessia", "Dave"]
///     print(employeesSet)
///     // Prints "["Amir", "Dave", "Jihye", "Alessia"]"
///
///     let employeesArray: [String] = ["Amir", "Jihye", "Dave", "Alessia", "Dave"]
///     print(employeesArray)
///     // Prints "["Amir", "Jihye", "Dave", "Alessia", "Dave"]"
///
/// The `Set` and `Array` types each handle array literals in their own way to
/// create new instances. In this case, the newly created set drops the
/// duplicate value ("Dave") and doesn't maintain the order of the array
/// literal's elements. The new array, on the other hand, matches the order
/// and number of elements provided.
///
/// - Note: An array literal is not the same as an `Array` instance. You can't
///   initialize a type that conforms to `ExpressibleByArrayLiteral` simply by
///   assigning an existing array.
///
///         let anotherSet: Set = employeesArray
///         // error: cannot convert value of type '[String]' to specified type 'Set'
///
/// Type Inference of Array Literals
/// ================================
///
/// Whenever possible, Swift's compiler infers the full intended type of your
/// array literal. Because `Array` is the default type for an array literal,
/// without writing any other code, you can declare an array with a particular
/// element type by providing one or more values.
///
/// In this example, the compiler infers the full type of each array literal.
///
///     let integers = [1, 2, 3]
///     // 'integers' has type '[Int]'
///
///     let strings = ["a", "b", "c"]
///     // 'strings' has type '[String]'
///
/// An empty array literal alone doesn't provide enough information for the
/// compiler to infer the intended type of the `Array` instance. When using an
/// empty array literal, specify the type of the variable or constant.
///
///     var emptyArray: [Bool] = []
///     // 'emptyArray' has type '[Bool]'
///
/// Because many functions and initializers fully specify the types of their
/// parameters, you can often use an array literal with or without elements as
/// a parameter. For example, the `sum(_:)` function shown here takes an `Int`
/// array as a parameter:
///
///     func sum(values: [Int]) -> Int {
///         return values.reduce(0, +)
///     }
///
///     let sumOfFour = sum([5, 10, 15, 20])
///     // 'sumOfFour' == 50
///
///     let sumOfNone = sum([])
///     // 'sumOfNone' == 0
///
/// When you call a function that does not fully specify its parameters' types,
/// use the type-cast operator (`as`) to specify the type of an array literal.
/// For example, the `log(name:value:)` function shown here has an
/// unconstrained generic `value` parameter.
///
///     func log<T>(name name: String, value: T) {
///         print("\(name): \(value)")
///     }
///
///     log(name: "Four integers", value: [5, 10, 15, 20])
///     // Prints "Four integers: [5, 10, 15, 20]"
///
///     log(name: "Zero integers", value: [] as [Int])
///     // Prints "Zero integers: []"
///
/// Conforming to ExpressibleByArrayLiteral
/// =======================================
///
/// Add the capability to be initialized with an array literal to your own
/// custom types by declaring an `init(arrayLiteral:)` initializer. The
/// following example shows the array literal initializer for a hypothetical
/// `OrderedSet` type, which has setlike semantics but maintains the order of
/// its elements.
///
///     struct OrderedSet<Element: Hashable>: Collection, SetAlgebra {
///         // implementation details
///     }
///
///     extension OrderedSet: ExpressibleByArrayLiteral {
///         init(arrayLiteral: Element...) {
///             self.init()
///             for element in arrayLiteral {
///                 self.append(element)
///             }
///         }
///     }
public protocol ExpressibleByArrayLiteral {
  /// The type of the elements of an array literal.
  associatedtype Element
  /// Creates an instance initialized with the given elements.
  init(arrayLiteral elements: Element...)
}

/// A type that can be initialized using a dictionary literal.
///
/// A dictionary literal is a simple way of writing a list of key-value pairs.
/// You write each key-value pair with a colon (`:`) separating the key and
/// the value. The dictionary literal is made up of one or more key-value
/// pairs, separated by commas and surrounded with square brackets.
///
/// To declare a dictionary, assign a dictionary literal to a variable or
/// constant:
///
///     let countryCodes = ["BR": "Brazil", "GH": "Ghana",
///                         "JP": "Japan", "US": "United States"]
///     // 'countryCodes' has type [String: String]
///
///     print(countryCodes["BR"]!)
///     // Prints "Brazil"
///
/// When the context provides enough type information, you can use a special
/// form of the dictionary literal, square brackets surrounding a single
/// colon, to initialize an empty dictionary.
///
///     var frequencies: [String: Int] = [:]
///     print(frequencies.count)
///     // Prints "0"
///
/// - Note: A dictionary literal is *not* the same as an instance of
///   `Dictionary` or the similarly named `DictionaryLiteral` type. You can't
///   initialize a type that conforms to `ExpressibleByDictionaryLiteral` simply
///   by assigning an instance of one of these types.
///
/// Conforming to the ExpressibleByDictionaryLiteral Protocol
/// =========================================================
///
/// To add the capability to be initialized with a dictionary literal to your
/// own custom types, declare an `init(dictionaryLiteral:)` initializer. The
/// following example shows the dictionary literal initializer for a
/// hypothetical `CountedSet` type, which uses setlike semantics while keeping
/// track of the count for duplicate elements:
///
///     struct CountedSet<Element: Hashable>: Collection, SetAlgebra {
///         // implementation details
///
///         /// Updates the count stored in the set for the given element,
///         /// adding the element if necessary.
///         ///
///         /// - Parameter n: The new count for `element`. `n` must be greater
///         ///   than or equal to zero.
///         /// - Parameter element: The element to set the new count on.
///         mutating func updateCount(_ n: Int, for element: Element)
///     }
///
///     extension CountedSet: ExpressibleByDictionaryLiteral {
///         init(dictionaryLiteral elements: (Element, Int)...) {
///             self.init()
///             for (element, count) in elements {
///                 self.updateCount(count, for: element)
///             }
///         }
///     }
public protocol ExpressibleByDictionaryLiteral {
  /// The key type of a dictionary literal.
  associatedtype Key
  /// The value type of a dictionary literal.
  associatedtype Value
  /// Creates an instance initialized with the given key-value pairs.
  init(dictionaryLiteral elements: (Key, Value)...)
}

/// A type that can be initialized by string interpolation with a string
/// literal that includes expressions.
///
/// Use string interpolation to include one or more expressions in a string
/// literal, wrapped in a set of parentheses and prefixed by a backslash. For
/// example:
///
///     let price = 2
///     let number = 3
///     let message = "One cookie: $\(price), \(number) cookies: $\(price * number)."
///     print(message)
///     // Prints "One cookie: $2, 3 cookies: $6."
///
/// Conforming to the ExpressibleByStringInterpolation Protocol
/// ===========================================================
///
/// To use string interpolation to initialize instances of your custom type,
/// implement the required initializers for `ExpressibleByStringInterpolation`
/// conformance. String interpolation is a multiple-step initialization
/// process. When you use string interpolation, the following steps occur:
///
/// 1. The string literal is broken into pieces. Each segment of the string
///    literal before, between, and after any included expressions, along with
///    the individual expressions themselves, are passed to the
///    `init(stringInterpolationSegment:)` initializer.
/// 2. The results of those calls are passed to the
///    `init(stringInterpolation:)` initializer in the order in which they
///    appear in the string literal.
///
/// In other words, initializing the `message` constant in the example above
/// using string interpolation is equivalent to the following code:
///
///     let message = String(stringInterpolation:
///           String(stringInterpolationSegment: "One cookie: $"),
///           String(stringInterpolationSegment: price),
///           String(stringInterpolationSegment: ", "),
///           String(stringInterpolationSegment: number),
///           String(stringInterpolationSegment: " cookies: $"),
///           String(stringInterpolationSegment: price * number),
///           String(stringInterpolationSegment: "."))
@available(*, deprecated, message: "it will be replaced or redesigned in Swift 4.0.  Instead of conforming to 'ExpressibleByStringInterpolation', consider adding an 'init(_:String)'")
public typealias ExpressibleByStringInterpolation = _ExpressibleByStringInterpolation
public protocol _ExpressibleByStringInterpolation {
  /// Creates an instance by concatenating the given values.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you use string interpolation. For example:
  ///
  ///     let s = "\(5) x \(2) = \(5 * 2)"
  ///     print(s)
  ///     // Prints "5 x 2 = 10"
  ///
  /// After calling `init(stringInterpolationSegment:)` with each segment of
  /// the string literal, this initializer is called with their string
  /// representations.
  ///
  /// - Parameter strings: An array of instances of the conforming type.
  init(stringInterpolation strings: Self...)
  
  /// Creates an instance containing the appropriate representation for the
  /// given value.
  ///
  /// Do not call this initializer directly. It is used by the compiler for
  /// each string interpolation segment when you use string interpolation. For
  /// example:
  ///
  ///     let s = "\(5) x \(2) = \(5 * 2)"
  ///     print(s)
  ///     // Prints "5 x 2 = 10"
  ///
  /// This initializer is called five times when processing the string literal
  /// in the example above; once each for the following: the integer `5`, the
  /// string `" x "`, the integer `2`, the string `" = "`, and the result of
  /// the expression `5 * 2`.
  ///
  /// - Parameter expr: The expression to represent.
  init<T>(stringInterpolationSegment expr: T)
}

/// A type that can be initialized using a color literal (e.g.
/// `#colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)`).
public protocol _ExpressibleByColorLiteral {
  /// Creates an instance initialized with the given properties of a color
  /// literal.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using a color literal.
  init(colorLiteralRed red: Float, green: Float, blue: Float, alpha: Float)
}

/// A type that can be initialized using an image literal (e.g.
/// `#imageLiteral(resourceName: "hi.png")`).
public protocol _ExpressibleByImageLiteral {
  /// Creates an instance initialized with the given resource name.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using an image literal.
  init(imageLiteralResourceName path: String)
}

/// A type that can be initialized using a file reference literal (e.g.
/// `#fileLiteral(resourceName: "resource.txt")`).
public protocol _ExpressibleByFileReferenceLiteral {
  /// Creates an instance initialized with the given resource name.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using a file reference literal.
  init(fileReferenceLiteralResourceName path: String)
}

/// A container is destructor safe if whether it may store to memory on
/// destruction only depends on its type parameters destructors.
/// For example, whether `Array<Element>` may store to memory on destruction
/// depends only on `Element`.
/// If `Element` is an `Int` we know the `Array<Int>` does not store to memory
/// during destruction. If `Element` is an arbitrary class
/// `Array<MemoryUnsafeDestructorClass>` then the compiler will deduce may
/// store to memory on destruction because `MemoryUnsafeDestructorClass`'s
/// destructor may store to memory on destruction.
/// If in this example during `Array`'s destructor we would call a method on any
/// type parameter - say `Element.extraCleanup()` - that could store to memory,
/// then Array would no longer be a _DestructorSafeContainer.
public protocol _DestructorSafeContainer {
}

@available(*, unavailable, renamed: "Bool")
public typealias BooleanType = Bool

// Deprecated by SE-0115.

@available(*, deprecated, renamed: "ExpressibleByNilLiteral")
public typealias NilLiteralConvertible
  = ExpressibleByNilLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinIntegerLiteral")
public typealias _BuiltinIntegerLiteralConvertible
  = _ExpressibleByBuiltinIntegerLiteral
@available(*, deprecated, renamed: "ExpressibleByIntegerLiteral")
public typealias IntegerLiteralConvertible
  = ExpressibleByIntegerLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinFloatLiteral")
public typealias _BuiltinFloatLiteralConvertible
  = _ExpressibleByBuiltinFloatLiteral
@available(*, deprecated, renamed: "ExpressibleByFloatLiteral")
public typealias FloatLiteralConvertible
  = ExpressibleByFloatLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinBooleanLiteral")
public typealias _BuiltinBooleanLiteralConvertible
  = _ExpressibleByBuiltinBooleanLiteral
@available(*, deprecated, renamed: "ExpressibleByBooleanLiteral")
public typealias BooleanLiteralConvertible
  = ExpressibleByBooleanLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinUnicodeScalarLiteral")
public typealias _BuiltinUnicodeScalarLiteralConvertible
  = _ExpressibleByBuiltinUnicodeScalarLiteral
@available(*, deprecated, renamed: "ExpressibleByUnicodeScalarLiteral")
public typealias UnicodeScalarLiteralConvertible
  = ExpressibleByUnicodeScalarLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinExtendedGraphemeClusterLiteral")
public typealias _BuiltinExtendedGraphemeClusterLiteralConvertible
  = _ExpressibleByBuiltinExtendedGraphemeClusterLiteral
@available(*, deprecated, renamed: "ExpressibleByExtendedGraphemeClusterLiteral")
public typealias ExtendedGraphemeClusterLiteralConvertible
  = ExpressibleByExtendedGraphemeClusterLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinStringLiteral")
public typealias _BuiltinStringLiteralConvertible
  = _ExpressibleByBuiltinStringLiteral
@available(*, deprecated, renamed: "_ExpressibleByBuiltinUTF16StringLiteral")
public typealias _BuiltinUTF16StringLiteralConvertible
  = _ExpressibleByBuiltinUTF16StringLiteral
@available(*, deprecated, renamed: "ExpressibleByStringLiteral")
public typealias StringLiteralConvertible
  = ExpressibleByStringLiteral
@available(*, deprecated, renamed: "ExpressibleByArrayLiteral")
public typealias ArrayLiteralConvertible
  = ExpressibleByArrayLiteral
@available(*, deprecated, renamed: "ExpressibleByDictionaryLiteral")
public typealias DictionaryLiteralConvertible
  = ExpressibleByDictionaryLiteral
@available(*, deprecated, message: "it will be replaced or redesigned in Swift 4.0.  Instead of conforming to 'StringInterpolationConvertible', consider adding an 'init(_:String)'")
public typealias StringInterpolationConvertible
  = ExpressibleByStringInterpolation
@available(*, deprecated, renamed: "_ExpressibleByColorLiteral")
public typealias _ColorLiteralConvertible
  = _ExpressibleByColorLiteral
@available(*, deprecated, renamed: "_ExpressibleByImageLiteral")
public typealias _ImageLiteralConvertible
  = _ExpressibleByImageLiteral
@available(*, deprecated, renamed: "_ExpressibleByFileReferenceLiteral")
public typealias _FileReferenceLiteralConvertible
  = _ExpressibleByFileReferenceLiteral

