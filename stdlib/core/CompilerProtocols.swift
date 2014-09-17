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

/// Protocol describing types that can be used as logical values within
/// a condition.
///
/// Types that conform to the `BooleanType` protocol can be used as
/// condition in various control statements (`if`, `while`, C-style
/// `for`) as well as other logical value contexts (e.g., `case`
/// statement guards).
public protocol BooleanType {
  var boolValue: Bool { get }
}

/// A `GeneratorType` is notionally a `SequenceType` that is consumed
/// when iterated.
///
/// While it is safe to copy a `GeneratorType`, only one copy should be advanced
/// with `next()`.
///
/// If an algorithm requires two `GeneratorType`\ s for the same
/// `SequenceType` to be advanced at the same time, and the specific
/// `SequenceType` type supports that, then those `GeneratorType`
/// objects should be obtained from `SequenceType` by two distinct
/// calls to `generate().  However in that case the algorithm should
/// probably require `CollectionType`, since `CollectionType` implies
/// multi-pass.
public protocol GeneratorType /* : SequenceType */ { 
  // FIXME: Refinement pending <rdar://problem/14396120>
  
  /// The type of which `Self` is a generator.
  typealias Element

  /// If all elements are exhausted, return `nil`.  Otherwise, advance
  /// to the next element and return it.
  ///
  /// Note: after `next()` on an arbitrary generator has returned
  /// `nil`, subsequent calls to `next()` have unspecified behavior.
  /// Specific implementations of this protocol are encouraged to
  /// respond by calling `preconditionFailure("...")`.
  mutating func next() -> Element?
}

/// The `for...in` loop operates on `SequenceType`\ s.  It is
/// unspecified whether `for...in` consumes the sequence on which it
/// operates.
public protocol _SequenceType {
}

public protocol _Sequence_Type : _SequenceType {
  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  typealias Generator : GeneratorType

  /// Return a generator over the elements of this sequence.  The
  /// generator's next element is the first element of the sequence.
  func generate() -> Generator
}

public protocol SequenceType : _Sequence_Type {
  typealias Generator : GeneratorType
  func generate() -> Generator

  func ~> (_:Self,_:(_UnderestimateCount,())) -> Int

  /// If `self` is multi-pass (i.e., a `CollectionType`), invoke the function
  /// on `self` and return its result.  Otherwise, return `nil`.
  func ~> <R>(_: Self, _: (_PreprocessingPass, ((Self)->R))) -> R?

  func ~>(
    _:Self, _: (_CopyToNativeArrayBuffer, ())
  ) -> _ContiguousArrayBuffer<Self.Generator.Element>
}

public struct _CopyToNativeArrayBuffer {}
public func _copyToNativeArrayBuffer<Args>(args: Args)
  -> (_CopyToNativeArrayBuffer, Args)
{
  return (_CopyToNativeArrayBuffer(), args)
}

// Operation tags for underestimateCount.  See Index.swift for an
// explanation of operation tags.
public struct _UnderestimateCount {}
internal func _underestimateCount<Args>(args: Args)
  -> (_UnderestimateCount, Args)
{
  return (_UnderestimateCount(), args)
}

// Default implementation of underestimateCount for Sequences.  Do not
// use this operator directly; call underestimateCount(s) instead
public func ~> <T: _SequenceType>(s: T,_:(_UnderestimateCount, ())) -> Int {
  return 0
}

/// Return an underestimate of the number of elements in the given
/// sequence, without consuming the sequence.  For Sequences that are
/// actually Collections, this will return countElements(x)
public func underestimateCount<T: SequenceType>(x: T) -> Int {
  return x~>_underestimateCount()
}

// Operation tags for preprocessingPass.  See Index.swift for an
// explanation of operation tags.
public struct _PreprocessingPass {}

// Default implementation of `_preprocessingPass` for Sequences.  Do not
// use this operator directly; call `_preprocessingPass(s)` instead
public func ~> <
  T : _SequenceType, R
>(s: T, _: (_PreprocessingPass, ( (T)->R ))) -> R? {
  return nil
}

internal func _preprocessingPass<Args>(args: Args)
  -> (_PreprocessingPass, Args)
{
  return (_PreprocessingPass(), args)
}

// Pending <rdar://problem/14011860> and <rdar://problem/14396120>,
// pass a GeneratorType through GeneratorSequence to give it "SequenceType-ness"
public struct GeneratorSequence<
  G: GeneratorType
> : GeneratorType, SequenceType {
  public init(_ base: G) {
    _base = base
  }
  
  public mutating func next() -> G.Element? {
    return _base.next()
  }

  public func generate() -> GeneratorSequence {
    return self
  }
  
  var _base: G
}

public protocol RawRepresentable {
  typealias RawValue
  init?(rawValue: RawValue)
  var rawValue: RawValue { get }
}

// Workaround for our lack of circular conformance checking. Allow == to be
// defined on _RawOptionSetType in order to satisfy the Equatable requirement of
// RawOptionSetType without a circularity our type-checker can't yet handle.
public protocol _RawOptionSetType: RawRepresentable, Equatable {
  typealias RawValue: BitwiseOperationsType, Equatable
  init(rawValue: RawValue)
}

public func == <T: _RawOptionSetType>(a: T, b: T) -> Bool {
  return a.rawValue == b.rawValue
}

public func & <T: _RawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue & b.rawValue)
}
public func | <T: _RawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue | b.rawValue)
}
public func ^ <T: _RawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue ^ b.rawValue)
}
public prefix func ~ <T: _RawOptionSetType>(a: T) -> T {
  return T(rawValue: ~a.rawValue)
}

// TODO: This is an incomplete implementation of our option sets vision.
public protocol RawOptionSetType : _RawOptionSetType, BitwiseOperationsType,
    NilLiteralConvertible {
  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  // The Clang importer synthesizes these for imported NS_OPTIONS.

  /* init?(rawValue: RawValue) { self.init(rawValue) } */
}

/// Conforming to this protocol allows a type to be usable with the 'nil'
/// literal.
public protocol NilLiteralConvertible {
  init(nilLiteral: ())
}

public protocol _BuiltinIntegerLiteralConvertible {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

public protocol IntegerLiteralConvertible {
  typealias IntegerLiteralType : _BuiltinIntegerLiteralConvertible
  init(integerLiteral value: IntegerLiteralType)
}

public protocol _BuiltinFloatLiteralConvertible {
  init(_builtinFloatLiteral value: _MaxBuiltinFloatType)
}

public protocol FloatLiteralConvertible {
  typealias FloatLiteralType : _BuiltinFloatLiteralConvertible
  init(floatLiteral value: FloatLiteralType)
}

public protocol _BuiltinBooleanLiteralConvertible {
  init(_builtinBooleanLiteral value: Builtin.Int1)
}

public protocol BooleanLiteralConvertible {
  typealias BooleanLiteralType : _BuiltinBooleanLiteralConvertible
  init(booleanLiteral value: BooleanLiteralType)
}

public protocol _BuiltinCharacterLiteralConvertible {
  init(_builtinCharacterLiteral value: Builtin.Int32)
}

public protocol CharacterLiteralConvertible {
  typealias CharacterLiteralType : _BuiltinCharacterLiteralConvertible
  init(characterLiteral value: CharacterLiteralType)
}

public protocol _BuiltinUnicodeScalarLiteralConvertible {
  class func _convertFromBuiltinUnicodeScalarLiteral(
    value: Builtin.Int32) -> Self
}

public protocol UnicodeScalarLiteralConvertible {
  typealias UnicodeScalarLiteralType : _BuiltinUnicodeScalarLiteralConvertible
  class func convertFromUnicodeScalarLiteral(
    value: UnicodeScalarLiteralType) -> Self
}

public protocol _BuiltinExtendedGraphemeClusterLiteralConvertible
  : _BuiltinUnicodeScalarLiteralConvertible {

  class func _convertFromBuiltinExtendedGraphemeClusterLiteral(
      start: Builtin.RawPointer,
      byteSize: Builtin.Word,
      isASCII: Builtin.Int1) -> Self
}

public protocol ExtendedGraphemeClusterLiteralConvertible
  : UnicodeScalarLiteralConvertible {

  typealias ExtendedGraphemeClusterLiteralType : _BuiltinExtendedGraphemeClusterLiteralConvertible
  class func convertFromExtendedGraphemeClusterLiteral(
      value: ExtendedGraphemeClusterLiteralType) -> Self
}

public protocol _BuiltinStringLiteralConvertible : _BuiltinExtendedGraphemeClusterLiteralConvertible {
  class func _convertFromBuiltinStringLiteral(start: Builtin.RawPointer,
                                              byteSize: Builtin.Word,
                                              isASCII: Builtin.Int1) -> Self
}

public protocol _BuiltinUTF16StringLiteralConvertible : _BuiltinStringLiteralConvertible {
  class func _convertFromBuiltinUTF16StringLiteral(
                start: Builtin.RawPointer,
                numberOfCodeUnits: Builtin.Word) -> Self
}

public protocol StringLiteralConvertible : ExtendedGraphemeClusterLiteralConvertible {
  // FIXME: when we have default function implementations in protocols, provide
  // an implementation of convertFromExtendedGraphemeClusterLiteral().

  typealias StringLiteralType : _BuiltinStringLiteralConvertible
  class func convertFromStringLiteral(value: StringLiteralType) -> Self
}

public protocol ArrayLiteralConvertible {
  typealias Element
  init(arrayLiteral elements: Element...)
}

public protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  init(dictionaryLiteral elements: (Key, Value)...)
}

public protocol StringInterpolationConvertible {
  class func convertFromStringInterpolation(strings: Self...) -> Self
  class func convertFromStringInterpolationSegment<T>(expr: T) -> Self
}

