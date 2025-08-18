//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A type that can be initialized with a Boolean literal.
///
/// The standard library Boolean type `Bool` conforms to this protocol.
public protocol ExpressibleByBooleanLiteral {
  /// A type that represents a Boolean literal.
  ///
  /// This type can be different from the conforming type, but the compiler
  /// must be able to implicitly convert instances of this type to the
  /// conforming type.
  associatedtype BooleanLiteralType

  /// Creates an instance initialized to the given Boolean value.
  ///
  /// Do not call this initializer directly. Instead, initialize a variable or
  /// constant using one of the Boolean literals `true` and `false`. For example:
  ///
  ///     let twasBrillig = true
  ///
  /// In this example, the assignment to the `twasBrillig` constant calls this
  /// Boolean literal initializer behind the scenes.
  ///
  /// - Parameter value: The value of the new instance.
  init(booleanLiteral value: BooleanLiteralType)
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
///     // An explicit type annotation is required to use 'UInt8'
///     let smallByte: UInt8 = 7
///
/// ## Conforming to ExpressibleByIntegerLiteral
///
/// To add `ExpressibleByIntegerLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByIntegerLiteral {
  /// A type that represents an integer literal.
  ///
  /// The standard library integer and floating-point types are all valid types
  /// for `IntegerLiteralType`.
  associatedtype IntegerLiteralType

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

/// A type that can be initialized with a floating-point literal.
///
/// The standard library floating-point types---`Float`, `Double`, and
/// `Float80` where available---all conform to the `ExpressibleByFloatLiteral`
/// protocol. You can initialize a variable of any of these types using a
/// floating-point literal.
///
///     // Type inferred as 'Double'
///     let threshold = 6.0
///
///     // An explicit type annotation is required to use 'Float'
///     let velocity: Float = 12.5
///
/// ## Conforming to ExpressibleByFloatLiteral
///
/// To add `ExpressibleByFloatLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByFloatLiteral {
  /// A type that represents a floating-point literal.
  ///
  /// Valid types for `FloatLiteralType` are `Float`, `Double`, and `Float80`
  /// where available.
  associatedtype FloatLiteralType

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

/// A type that can be initialized using a string literal.
///
/// The `String` and `StaticString` types conform to the
/// `ExpressibleByStringLiteral` protocol. You can initialize a variable of
/// either of these types using a string literal of any length.
///
///     let picnicGuest = "Deserving porcupine"
///
/// ## Conforming to ExpressibleByStringLiteral
///
/// To add `ExpressibleByStringLiteral` conformance to your custom type,
/// implement the required initializers. String literals in Swift are
/// Unicode-correct, so your initializers must be prepared to accept Unicode
/// content. During compilation, string literals are broken down into a series
/// of Unicode extended grapheme clusters, the largest unit of human-readable
/// text.
public protocol ExpressibleByStringLiteral: ExpressibleByExtendedGraphemeClusterLiteral {
  /// A type that represents a string literal.
  ///
  /// Valid types for `StringLiteralType` are `String` and `StaticString`.
  associatedtype StringLiteralType

  /// Creates an instance initialized to the given string value.
  ///
  /// - Parameter value: The value of the new instance.
  init(stringLiteral value: StringLiteralType)
}

/// A type that can be initialized using a single Unicode extended grapheme
/// cluster.
///
/// An *extended grapheme cluster* is a group of one or more Unicode code
/// points that approximates a single user-perceived character. Many individual
/// characters, such as "é", "김", and "🇮🇳", can be made up of multiple Unicode
/// code points. These code points are combined by Unicode's boundary and
/// normalization algorithms into extended grapheme clusters.
///
/// The `Character` and `String` types conform to the
/// `ExpressibleByExtendedGraphemeClusterLiteral` protocol. You can initialize a
/// variable of either of these types using a single character.
///
///     let snowflake: Character = "❄︎"
///     let unicorn: String = "🦄"
///
/// ## Conforming to ExpressibleByExtendedGraphemeClusterLiteral
///
/// To add `ExpressibleByExtendedGraphemeClusterLiteral` conformance to your
/// custom type, implement the required initializer.
public protocol ExpressibleByExtendedGraphemeClusterLiteral: ExpressibleByUnicodeScalarLiteral {
  /// A type that represents an extended grapheme cluster literal.
  ///
  /// Valid types for `ExtendedGraphemeClusterLiteralType` are `Character`,
  /// `String`, and `StaticString`.
  associatedtype ExtendedGraphemeClusterLiteralType

  /// Creates an instance initialized to the given extended grapheme cluster.
  ///
  /// - Parameter value: The value of the new instance.
  init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType)
}

/// A type that can be initialized using a Unicode scalar value.
///
/// A Unicode scalar value is represented by a `Unicode.Scalar` instance.
/// `Unicode.Scalar` instances are equivalent to UTF-32 code units.
///
/// The `Character`, `String`, and `Unicode.Scalar` types conform to the
/// `ExpressibleByUnicodeScalarLiteral` protocol. You can initialize a variable
/// or constant of any of these types using a string literal that holds a
/// single Unicode scalar value.
///
///     let letterK: Character = "K"
///     let unicodeK: Unicode.Scalar = "K"
///
/// ## Conforming to ExpressibleByUnicodeScalarLiteral
///
/// To add `ExpressibleByUnicodeScalarLiteral` conformance to your custom type,
/// implement the required initializer.
public protocol ExpressibleByUnicodeScalarLiteral {
  /// A type that represents a Unicode scalar literal.
  ///
  /// Valid types for `UnicodeScalarLiteralType` are `Unicode.Scalar`,
  /// `Character`, `String`, and `StaticString`.
  associatedtype UnicodeScalarLiteralType

  /// Creates an instance initialized to the given Unicode scalar value.
  ///
  /// - Parameter value: The value of the new instance.
  init(unicodeScalarLiteral value: UnicodeScalarLiteralType)
}

/// A type that can be initialized using an array literal.
///
/// An array literal is a simple way of expressing a list of values. Simply
/// surround a comma-separated list of values, instances, or literals with
/// square brackets to create an array literal. You can use an array literal
/// anywhere an instance of an `ExpressibleByArrayLiteral` type is expected: as
/// a value assigned to a variable or constant, as a parameter to a method or
/// initializer, or even as the subject of a nonmutating operation.
///
/// Arrays, sets, and option sets all conform to `ExpressibleByArrayLiteral`,
/// and your own custom types can as well. Here's an example of creating a set
/// and an array using array literals:
///
///     let employeesSet: Set = ["Amir", "Jihye", "Dave", "Alessia", "Dave"]
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
/// literal's elements. The new array, on the other hand, matches the order and
/// number of elements provided.
///
/// ## Type Inference of Array Literals
///
/// Whenever possible, Swift's compiler infers the full intended type of your
/// array literal. Because `Array` is the default type for an array literal,
/// without writing any other code, you can declare an array with a particular
/// element type by providing one or more values.
///
///     let integers = [1, 2, 3]
///     // 'integers' has type '[Int]'
///
///     let strings = ["a", "b", "c"]
///     // 'strings' has type '[String]'
///
/// If you want to create an array literal with a type other than `Array`---for
/// example, to create a `Set`---use a type annotation or specify the type with
/// a declaration.
///
///     let employeesSet: Set<String> = ["Amir", "Jihye", "Dave", "Alessia", "Dave"]
///     let employeesArray: Array<String> = ["Amir", "Jihye", "Dave", "Alessia", "Dave"]
///
/// ## Conforming to ExpressibleByArrayLiteral
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
  associatedtype ArrayLiteralElement

  /// Creates an instance initialized with the given elements.
  init(arrayLiteral elements: ArrayLiteralElement...)
}

/// A type that can be initialized using a dictionary literal.
///
/// A dictionary literal is a simple way of writing a list of key-value pairs.
/// You write each key-value pair with a colon (`:`) separating the key and
/// value. The dictionary literal is made up of one or more key-value pairs,
/// separated by commas and surrounded with square brackets.
///
/// To declare a dictionary, assign a dictionary literal to a variable or
/// constant:
///
///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
///     print(countryCodes)
///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
///
/// When the context provides enough type information, you can use a dictionary
/// literal with an empty list of key-value pairs to create an empty dictionary.
/// For example, if you are assigning a dictionary literal to a variable with
/// an explicit `[String: Int]` type annotation, you can assign an empty
/// dictionary with `[:]`.
///
///     var frequencies: [String: Int] = [:]
///     print(frequencies.isEmpty)
///     // Prints "true"
///
/// ## Type Inference of Dictionary Literals
///
/// When you use a dictionary literal, Swift's compiler attempts to infer the
/// types for the keys and values by examining what you've written. In this
/// playground example, the compiler infers that both the keys and values of
/// the `responses` dictionary are of type `String`.
///
///     let responses = ["language": "Swift", "tool": "Xcode"]
///     // 'responses' has type '[String: String]'
///
/// If you want to control the type of a dictionary literal, provide a type
/// annotation when you create the dictionary.
///
/// ## Conforming to ExpressibleByDictionaryLiteral
///
/// To add the capability to be initialized with a dictionary literal to your
/// own custom types, declare an `init(dictionaryLiteral:)` initializer.
public protocol ExpressibleByDictionaryLiteral {
  /// The key type of a dictionary literal.
  associatedtype Key

  /// The value type of a dictionary literal.
  associatedtype Value

  /// Creates an instance initialized with the given key-value pairs.
  init(dictionaryLiteral elements: (Key, Value)...)
}

/// A type that can be initialized using the nil literal, `nil`.
///
/// `nil` has a specific meaning in Swift---the absence of a value. Only the
/// `Optional` type conforms to `ExpressibleByNilLiteral`.
/// `ExpressibleByNilLiteral` conformance for types that use `nil` for other
/// purposes is discouraged.
///
/// ## Conforming to ExpressibleByNilLiteral
///
/// To add `ExpressibleByNilLiteral` conformance to your custom type, implement
/// the required `init(nilLiteral:)` initializer.
public protocol ExpressibleByNilLiteral {
  /// Creates an instance initialized with `nil`.
  ///
  /// - Parameter nilLiteral: An empty tuple literal.
  init(nilLiteral: ())
}