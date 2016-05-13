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
// Intrinsic protocols shared with the compiler
//===----------------------------------------------------------------------===//

/// A type that represents a Boolean value.
///
/// Types that conform to the `Boolean` protocol can be used as the condition
/// in control statements, such as `if` and `while`, and in other contexts
/// that require a logical value, such as the `where` clause of a `case`
/// statement.
///
/// Swift uses only simple Boolean values in conditional contexts to help avoid
/// accidental programming errors and to help maintain the clarity of each
/// control statement. Unlike other programming languages, integers or strings
/// cannot be used where a Boolean value is expected.
///
/// For example, the following code sample will not compile, because it
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
/// Conforming to the Boolean Protocol
/// ==================================
///
/// Only three types provided by Swift---`Bool`, `DarwinBoolean`, and
/// `ObjCBool`---conform to the `Boolean` protocol. Expanding this set to
/// include types that represent more than simple Boolean values is
/// discouraged.
///
/// To add `Boolean` conformance to your custom type, implement a `boolValue`
/// property that represents your type as an instance of `Bool`, the default
/// concrete type for the `Boolean` protocol.
public protocol Boolean {
  /// This value expressed as a `Bool` instance.
  var boolValue: Bool { get }
}

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

/// Returns a Boolean value indicating whether the two operands are equal.
///
/// - Parameters:
///   - lhs: A raw-representable instance.
///   - rhs: A second raw-representable instance.
/// - Returns: `true` if the two operands have equal raw values; otherwise,
///   `false`.
@warn_unused_result
public func == <
  T : RawRepresentable where T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue == rhs.rawValue
}

/// Returns a Boolean value indicating whether the two operands are not equal.
///
/// - Parameters:
///   - lhs: A raw-representable instance.
///   - rhs: A second raw-representable instance.
/// - Returns: `true` if the two operands have unequal raw values; otherwise,
///   `false`.
@warn_unused_result
public func != <
  T : RawRepresentable where T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue != rhs.rawValue
}

// This overload is needed for ambiguity resolution against the
// implementation of != for T : Equatable
/// Returns a Boolean value indicating whether the two operands are not equal.
///
/// - Parameters:
///   - lhs: A raw-representable instance.
///   - rhs: A second raw-representable instance.
/// - Returns: `true` if the two operands have unequal raw values; otherwise,
///   `false`.
@warn_unused_result
public func != <
  T : Equatable where T : RawRepresentable, T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue != rhs.rawValue
}

/// A type that can be initialized using the nil literal, `nil`.
///
/// `nil` has a specific meaning in Swift---the absence of a value. Only the
/// `Optional` type conforms to `NilLiteralConvertible`.
/// `NilLiteralConvertible` conformance for types that use `nil` for other
/// purposes is discouraged.
///
/// - SeeAlso: `Optional`
public protocol NilLiteralConvertible {
  /// Creates an instance initialized with `nil`.
  init(nilLiteral: ())
}

public protocol _BuiltinIntegerLiteralConvertible {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

/// Conforming types can be initialized with integer literals.
public protocol IntegerLiteralConvertible {
  associatedtype IntegerLiteralType : _BuiltinIntegerLiteralConvertible
  /// Create an instance initialized to `value`.
  init(integerLiteral value: IntegerLiteralType)
}

public protocol _BuiltinFloatLiteralConvertible {
  init(_builtinFloatLiteral value: _MaxBuiltinFloatType)
}

/// Conforming types can be initialized with floating point literals.
public protocol FloatLiteralConvertible {
  associatedtype FloatLiteralType : _BuiltinFloatLiteralConvertible
  /// Create an instance initialized to `value`.
  init(floatLiteral value: FloatLiteralType)
}

public protocol _BuiltinBooleanLiteralConvertible {
  init(_builtinBooleanLiteral value: Builtin.Int1)
}

/// A type that can be initialized with the Boolean literals `true` and
/// `false`.
///
/// Only three types provided by Swift---`Bool`, `DarwinBoolean`, and
/// `ObjCBool`---are treated as Boolean values. Expanding this set to include
/// types that represent more than simple Boolean values is discouraged.
///
/// To add `BooleanLiteralConvertible` conformance to your custom type,
/// implement the `init(booleanLiteral:)` initializer that creates an instance
/// of your type with the given Boolean value.
public protocol BooleanLiteralConvertible {
  /// A type that can represent a Boolean literal, such as `Bool`.
  associatedtype BooleanLiteralType : _BuiltinBooleanLiteralConvertible

  /// Creates an instance initialized to to the given Boolean value.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using one of the Boolean literals `true` and `false`. For
  /// example:
  ///
  ///     let twasBrillig = true
  ///
  /// In this example, the assignments to the `twasBrillig` constant calls this
  /// Boolean literal initializer behind the scenes.
  ///
  /// - Parameter value: The value of the new instance.
  init(booleanLiteral value: BooleanLiteralType)
}

public protocol _BuiltinUnicodeScalarLiteralConvertible {
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
}

/// Conforming types can be initialized with string literals
/// containing a single [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
public protocol UnicodeScalarLiteralConvertible {
  associatedtype UnicodeScalarLiteralType : _BuiltinUnicodeScalarLiteralConvertible
  /// Create an instance initialized to `value`.
  init(unicodeScalarLiteral value: UnicodeScalarLiteralType)
}

public protocol _BuiltinExtendedGraphemeClusterLiteralConvertible
  : _BuiltinUnicodeScalarLiteralConvertible {

  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1)
}

/// Conforming types can be initialized with string literals
/// containing a single [Unicode extended grapheme cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster).
public protocol ExtendedGraphemeClusterLiteralConvertible
  : UnicodeScalarLiteralConvertible {

  associatedtype ExtendedGraphemeClusterLiteralType
    : _BuiltinExtendedGraphemeClusterLiteralConvertible
  /// Create an instance initialized to `value`.
  init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType)
}

public protocol _BuiltinStringLiteralConvertible
  : _BuiltinExtendedGraphemeClusterLiteralConvertible {

  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1)
}

public protocol _BuiltinUTF16StringLiteralConvertible
  : _BuiltinStringLiteralConvertible {

  init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word)
}

/// Conforming types can be initialized with arbitrary string literals.
public protocol StringLiteralConvertible
  : ExtendedGraphemeClusterLiteralConvertible {
  // FIXME: when we have default function implementations in protocols, provide
  // an implementation of init(extendedGraphemeClusterLiteral:).

  associatedtype StringLiteralType : _BuiltinStringLiteralConvertible
  /// Create an instance initialized to `value`.
  init(stringLiteral value: StringLiteralType)
}

/// A type that can be initialized using an array literal.
///
/// An array literal is a simple way of expressing a list of values. Simply
/// surround a comma-separated list of values, instances, or literals with
/// square brackets to create an array literal. You can use an array literal
/// anywhere an instance of an `ArrayLiteralConvertible` type is expected: as
/// a value assigned to a variable or constant, as a parameter to a method or
/// initializer, or even as the subject of a nonmutating operation like
/// `map(_:)` or `filter(_:)`.
///
/// Arrays, sets, and option sets all conform to `ArrayLiteralConvertible`, and
/// your own custom types can as well. Here's an example of creating a set and
/// an array using array literals:
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
///   initialize a type that conforms to `ArrayLiteralConvertible` simply by
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
///         return values.reduce(0, combine: +)
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
/// Conforming to ArrayLiteralConvertible
/// =====================================
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
///     extension OrderedSet: ArrayLiteralConvertible {
///         init(arrayLiteral: Element...) {
///             self.init()
///             for element in arrayLiteral {
///                 self.append(element)
///             }
///         }
///     }
public protocol ArrayLiteralConvertible {
  associatedtype Element
  /// Creates an instance initialized with the given elements.
  init(arrayLiteral elements: Element...)
}

/// Conforming types can be initialized with dictionary literals.
public protocol DictionaryLiteralConvertible {
  associatedtype Key
  associatedtype Value
  /// Create an instance initialized with `elements`.
  init(dictionaryLiteral elements: (Key, Value)...)
}

/// Conforming types can be initialized with string interpolations
/// containing `\(`...`)` clauses.
public protocol StringInterpolationConvertible {
  /// Create an instance by concatenating the elements of `strings`.
  init(stringInterpolation strings: Self...)
  /// Create an instance containing `expr`'s `print` representation.
  init<T>(stringInterpolationSegment expr: T)
}

/// Conforming types can be initialized with color literals (e.g.
/// `#colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)`).
public protocol _ColorLiteralConvertible {
  init(colorLiteralRed red: Float, green: Float, blue: Float, alpha: Float)
}

/// Conforming types can be initialized with image literals (e.g.
/// `#imageLiteral(resourceName: "hi.png")`).
public protocol _ImageLiteralConvertible {
  init(imageLiteralResourceName path: String)
}

/// Conforming types can be initialized with strings (e.g.
/// `#fileLiteral(resourceName: "resource.txt")`).
public protocol _FileReferenceLiteralConvertible {
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

@available(*, unavailable, renamed: "Boolean")
public typealias BooleanType = Boolean
