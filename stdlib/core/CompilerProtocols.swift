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

/// Protocol describing types that can be used as array bounds.
///
/// Types that conform to the `ArrayBound` protocol can be used as array bounds
/// by providing an operation (`getArrayBoundValue`) that produces an integral
/// value.
public protocol ArrayBound {
  typealias ArrayBoundType
  func getArrayBoundValue() -> ArrayBoundType
}

/// Protocol describing types that can be used as logical values within
/// a condition.
///
/// Types that conform to the `LogicValue` protocol can be used as
/// condition in various control statements (`if`, `while`, C-style
/// `for`) as well as other logical value contexts (e.g., `case`
/// statement guards).
public protocol LogicValue {
  func getLogicValue() -> Bool
}

/// A `Generator` is a `Sequence` that is consumed when iterated.
///
/// While it is safe to copy a `Generator`, only one copy should be advanced
/// with `next()`.
///
/// If an algorithm requires two `Generator`\ s for the same `Sequence` to be
/// advanced at the same time, and the specific `Sequence` type supports
/// that, then those `Generator` objects should be obtained from `Sequence` by
/// two distinct calls to `generate().  However in that case the algorithm
/// should probably require `Collection`, since `Collection` implies
/// multi-pass.
public protocol Generator /* : Sequence */ { 
  // FIXME: Refinement pending <rdar://problem/14396120>
  typealias Element
  mutating func next() -> Element?
}

/// The `for...in` loop operates on `Sequence`\ s.  It is unspecified whether
/// `for...in` consumes the sequence on which it operates.
public protocol _Sequence {
}

public protocol _Sequence_ : _Sequence {
  typealias GeneratorType : Generator
  func generate() -> GeneratorType
}

public protocol Sequence : _Sequence_ {
  typealias GeneratorType : Generator
  func generate() -> GeneratorType

  func ~> (_:Self,_:(_UnderestimateCount,())) -> Int

  /// If `self` is multi-pass (i.e., a `Collection`), invoke the function
  /// on `self` and return its result.  Otherwise, return `nil`.
  func ~> <R>(_: Self, _: (_PreprocessingPass, ((Self)->R))) -> R?

  func ~>(
    _:Self, _: (_CopyToNativeArrayBuffer, ())
  ) -> _ContiguousArrayBuffer<Self.GeneratorType.Element>
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
public func ~> <T: _Sequence>(s: T,_:(_UnderestimateCount, ())) -> Int {
  return 0
}

/// Return an underestimate of the number of elements in the given
/// sequence, without consuming the sequence.  For Sequences that are
/// actually Collections, this will return countElements(x)
public func underestimateCount<T: Sequence>(x: T) -> Int {
  return x~>_underestimateCount()
}

// Operation tags for preprocessingPass.  See Index.swift for an
// explanation of operation tags.
public struct _PreprocessingPass {}

// Default implementation of `_preprocessingPass` for Sequences.  Do not
// use this operator directly; call `_preprocessingPass(s)` instead
public func ~> <
  T : _Sequence, R
>(s: T, _: (_PreprocessingPass, ( (T)->R ))) -> R? {
  return nil
}

internal func _preprocessingPass<Args>(args: Args)
  -> (_PreprocessingPass, Args)
{
  return (_PreprocessingPass(), args)
}

// Pending <rdar://problem/14011860> and <rdar://problem/14396120>,
// pass a Generator through GeneratorSequence to give it "Sequence-ness"
public struct GeneratorSequence<G: Generator> : Generator, Sequence {
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
  typealias RawType
  class func fromRaw(raw: RawType) -> Self?
  func toRaw() -> RawType
}

// Workaround for our lack of circular conformance checking. Allow == to be
// defined on _RawOptionSet in order to satisfy the Equatable requirement of
// RawOptionSet without a circularity our type-checker can't yet handle.
public protocol _RawOptionSet: RawRepresentable {
  typealias RawType : BitwiseOperations, Equatable
}

// TODO: This is an incomplete implementation of our option sets vision.
public protocol RawOptionSet : _RawOptionSet, LogicValue, Equatable,
                                NilLiteralConvertible {
  // A non-failable version of RawRepresentable.fromRaw.
  class func fromMask(raw: RawType) -> Self

  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  // The Clang importer synthesizes these for imported NS_OPTIONS.

  /* class func fromRaw(raw: RawType) -> Self? { return fromMask(raw) } */

  /* func getLogicValue() -> Bool { return toRaw() != .allZeros() } */
}

/// Conforming to this protocol allows a type to be usable with the 'nil'
/// literal.
public protocol NilLiteralConvertible {
  class func convertFromNilLiteral() -> Self
}

public protocol _BuiltinIntegerLiteralConvertible {
  class func _convertFromBuiltinIntegerLiteral(
                value: MaxBuiltinIntegerType) -> Self
}

public protocol IntegerLiteralConvertible {
  typealias IntegerLiteralType : _BuiltinIntegerLiteralConvertible
  class func convertFromIntegerLiteral(value: IntegerLiteralType) -> Self
}

public protocol _BuiltinFloatLiteralConvertible {
  class func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

public protocol FloatLiteralConvertible {
  typealias FloatLiteralType : _BuiltinFloatLiteralConvertible
  class func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}

public protocol _BuiltinBooleanLiteralConvertible {
  class func _convertFromBuiltinBooleanLiteral(
                value: Builtin.Int1) -> Self
}

public protocol BooleanLiteralConvertible {
  typealias BooleanLiteralType : _BuiltinBooleanLiteralConvertible
  class func convertFromBooleanLiteral(value: BooleanLiteralType) -> Self
}

public protocol _BuiltinCharacterLiteralConvertible {
  class func _convertFromBuiltinCharacterLiteral(value: Builtin.Int32) -> Self
}

public protocol CharacterLiteralConvertible {
  typealias CharacterLiteralType : _BuiltinCharacterLiteralConvertible
  class func convertFromCharacterLiteral(value: CharacterLiteralType) -> Self
}

public protocol _BuiltinExtendedGraphemeClusterLiteralConvertible {
  class func _convertFromBuiltinExtendedGraphemeClusterLiteral(
      start: Builtin.RawPointer,
      byteSize: Builtin.Word,
      isASCII: Builtin.Int1) -> Self
}

public protocol ExtendedGraphemeClusterLiteralConvertible {
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
  class func convertFromArrayLiteral(elements: Element...) -> Self
}

public protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  class func convertFromDictionaryLiteral(elements: (Key, Value)...) -> Self
}

public protocol StringInterpolationConvertible {
  class func convertFromStringInterpolation(strings: Self...) -> Self
  class func convertFromStringInterpolationSegment<T>(expr: T) -> Self
}

