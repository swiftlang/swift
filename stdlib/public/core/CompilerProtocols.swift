//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Intrinsic protocols shared with the compiler
//===----------------------------------------------------------------------===//

/// A type that represents a boolean value.
///
/// Types that conform to the `BooleanType` protocol can be used as
/// the condition in control statements (`if`, `while`, C-style `for`)
/// and other logical value contexts (e.g., `case` statement guards).
///
/// Only two types provided by Swift, `Bool` and `ObjCBool`, conform
/// to `BooleanType`. Expanding this set to include types that
/// represent more than simple boolean values is discouraged.
public protocol BooleanType {
  /// The value of `self`, expressed as a `Bool`.
  var boolValue: Bool { get }
}

/// A type that can be converted to an associated "raw" type, then
/// converted back to produce an instance equivalent to the original.
public protocol RawRepresentable {
  /// The "raw" type that can be used to represent all values of `Self`.
  ///
  /// Every distinct value of `self` has a corresponding unique
  /// value of `RawValue`, but `RawValue` may have representations
  /// that do not correspond to an value of `Self`.
  typealias RawValue

  /// Convert from a value of `RawValue`, yielding `nil` iff
  /// `rawValue` does not correspond to a value of `Self`.
  init?(rawValue: RawValue)

  /// The corresponding value of the "raw" type.
  ///
  /// `Self(rawValue: self.rawValue)!` is equivalent to `self`.
  var rawValue: RawValue { get }
}

// Workaround for our lack of circular conformance checking. Allow == to be
// defined on _RawOptionSetType in order to satisfy the Equatable requirement of
// RawOptionSetType without a circularity our type-checker can't yet handle.

/// This protocol is an implementation detail of `RawOptionSetType`; do
/// not use it directly.
///
/// Its requirements are inherited by `RawOptionSetType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _RawOptionSetType : RawRepresentable, Equatable {
  typealias RawValue : BitwiseOperationsType, Equatable
  init(rawValue: RawValue)
}

public func == <T : _RawOptionSetType>(a: T, b: T) -> Bool {
  return a.rawValue == b.rawValue
}

public func & <T : _RawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue & b.rawValue)
}
public func | <T : _RawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue | b.rawValue)
}
public func ^ <T : _RawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue ^ b.rawValue)
}
public prefix func ~ <T : _RawOptionSetType>(a: T) -> T {
  return T(rawValue: ~a.rawValue)
}

/// Protocol for `NS_OPTIONS` imported from Objective-C
public protocol RawOptionSetType : _RawOptionSetType, BitwiseOperationsType,
    NilLiteralConvertible {
  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  // The Clang importer synthesizes these for imported NS_OPTIONS.

  /* init?(rawValue: RawValue) { self.init(rawValue) } */
}

/// Conforming types can be initialized with `nil`.
public protocol NilLiteralConvertible {
  /// Create an instance initialized with `nil`.
  init(nilLiteral: ())
}

public protocol _BuiltinIntegerLiteralConvertible {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

/// Conforming types can be initialized with integer literals
public protocol IntegerLiteralConvertible {
  typealias IntegerLiteralType : _BuiltinIntegerLiteralConvertible
  /// Create an instance initialized to `value`.
  init(integerLiteral value: IntegerLiteralType)
}

public protocol _BuiltinFloatLiteralConvertible {
  init(_builtinFloatLiteral value: _MaxBuiltinFloatType)
}

/// Conforming types can be initialized with floating point literals
public protocol FloatLiteralConvertible {
  typealias FloatLiteralType : _BuiltinFloatLiteralConvertible
  /// Create an instance initialized to `value`.
  init(floatLiteral value: FloatLiteralType)
}

public protocol _BuiltinBooleanLiteralConvertible {
  init(_builtinBooleanLiteral value: Builtin.Int1)
}

/// Conforming types can be initialized with the boolean literals
/// `true` and `false`.
public protocol BooleanLiteralConvertible {
  typealias BooleanLiteralType : _BuiltinBooleanLiteralConvertible
  /// Create an instance initialized to `value`.
  init(booleanLiteral value: BooleanLiteralType)
}

internal protocol _BuiltinCharacterLiteralConvertible {
  init(_builtinCharacterLiteral value: Builtin.Int32)
}

internal protocol CharacterLiteralConvertible {
  typealias CharacterLiteralType : _BuiltinCharacterLiteralConvertible
  /// Create an instance initialized to `value`.
  init(characterLiteral value: CharacterLiteralType)
}

public protocol _BuiltinUnicodeScalarLiteralConvertible {
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
}

/// Conforming types can be initialized with string literals
/// containing a single [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
public protocol UnicodeScalarLiteralConvertible {
  typealias UnicodeScalarLiteralType : _BuiltinUnicodeScalarLiteralConvertible
  /// Create an instance initialized to `value`.
  init(unicodeScalarLiteral value: UnicodeScalarLiteralType)
}

public protocol _BuiltinExtendedGraphemeClusterLiteralConvertible
  : _BuiltinUnicodeScalarLiteralConvertible {

  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1)
}

/// Conforming types can be initialized with string literals
/// containing a single [Unicode extended grapheme cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster).
public protocol ExtendedGraphemeClusterLiteralConvertible
  : UnicodeScalarLiteralConvertible {

  typealias ExtendedGraphemeClusterLiteralType
    : _BuiltinExtendedGraphemeClusterLiteralConvertible
  /// Create an instance initialized to `value`.
  init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType)
}

public protocol _BuiltinStringLiteralConvertible
  : _BuiltinExtendedGraphemeClusterLiteralConvertible {

  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1)
}

public protocol _BuiltinUTF16StringLiteralConvertible
  : _BuiltinStringLiteralConvertible {

  init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    numberOfCodeUnits: Builtin.Word)
}

/// Conforming types can be initialized with arbitrary string literals
public protocol StringLiteralConvertible
  : ExtendedGraphemeClusterLiteralConvertible {
  // FIXME: when we have default function implementations in protocols, provide
  // an implementation of init(extendedGraphemeClusterLiteral:).

  typealias StringLiteralType : _BuiltinStringLiteralConvertible
  /// Create an instance initialized to `value`.
  init(stringLiteral value: StringLiteralType)
}

/// Conforming types can be initialized with array literals
public protocol ArrayLiteralConvertible {
  typealias Element
  /// Create an instance initialized with `elements`.
  init(arrayLiteral elements: Element...)
}

/// Conforming types can be initialized with dictionary literals
public protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  /// Create an instance initialized with `elements`.
  init(dictionaryLiteral elements: (Key, Value)...)
}

/// Conforming types can be initialized with string interpolations
/// containing `\(`...`)` clauses.
public protocol StringInterpolationConvertible {
  /// Create an instance by concatenating the elements of `strings`
  init(stringInterpolation strings: Self...)
  /// Create an instance containing `expr`'s `print` representation
  init<T>(stringInterpolationSegment expr: T)
}

/// Conforming types can be initialized with color literals (e.g.
/// [#Color(colorLiteralRed: 1, blue: 0, green: 0, alpha: 1)#]).
public protocol _ColorLiteralConvertible {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)
}

/// Optionals of conforming types can be initialized with image literals (e.g.
/// [#Image(imageLiteral: "hi.png")#]).
public protocol _ImageLiteralConvertible {
  init?(imageLiteral: String)
}

/// A container is destructor safe if whether it may store to memory on
/// destruction only depends on its type parameters.
/// For example, whether Array<T> may store to memory on destruction depends
/// only on T.
/// If T is an Int we know the Array<Int> does not store to memory during
/// destruction. If T is an arbitrary class Array<MemoryUnsafeDestructorClass>
/// then the compiler will deduce may store to memory on destruction because
/// MemoryUnsafeDestructorClass' destructor may store to memory on destruction.
public protocol _DestructorSafeContainer {
}
