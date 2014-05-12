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
protocol ArrayBound {
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
protocol LogicValue {
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
protocol Generator /* : Sequence */ { 
  // FIXME: Refinement pending <rdar://problem/14396120>
  typealias Element
  mutating func next() -> Element?
}

/// The `for...in` loop operates on `Sequence`\ s.  It is unspecified whether
/// `for...in` consumes the sequence on which it operates.
protocol _Sequence {
}

protocol _Sequence_ : _Sequence {
  typealias GeneratorType : Generator
  func generate() -> GeneratorType
}

protocol Sequence : _Sequence_ {
  typealias GeneratorType : Generator
  func generate() -> GeneratorType
  
  func ~> (_:Self,_:(_UnderestimateCount,())) -> Int
  
  func ~>(
    _:Self, _: (_CopyToNativeArrayBuffer, ())
  ) -> NativeArrayBuffer<Self.GeneratorType.Element>
}

struct _CopyToNativeArrayBuffer {}
func _copyToNativeArrayBuffer<Args>(args: Args)
  -> (_CopyToNativeArrayBuffer, Args)
{
  return (_CopyToNativeArrayBuffer(), args)
}

// Operation tags for underestimateCount.  See Index.swift for an
// explanation of operation tags.
struct _UnderestimateCount {}
func _underestimateCount<Args>(args: Args) -> (_UnderestimateCount, Args) {
  return (_UnderestimateCount(), args)
}

// Default implementation of underestimateCount for Sequences.  Do not
// use this operator directly; call underestimateCount(s) instead
func ~> <T: _Sequence>(s: T,_:(_UnderestimateCount, ())) -> Int {
  return 0
}

/// Return an underestimate of the number of elements in the given
/// sequence, without consuming the sequence.  For Sequences that are
/// actually Collections, this will return countElements(x)
func underestimateCount<T: Sequence>(x: T) -> Int {
  return x~>_underestimateCount()
}

// Pending <rdar://problem/14011860> and <rdar://problem/14396120>,
// pass a Generator through GeneratorSequence to give it "Sequence-ness"
struct GeneratorSequence<G: Generator> : Generator, Sequence {
  init(_ base: G) {
    _base = base
  }
  
  mutating func next() -> G.Element? {
    return _base.next()
  }

  func generate() -> GeneratorSequence {
    return self
  }
  
  var _base: G
}

protocol RawRepresentable {
  typealias RawType
  class func fromRaw(raw: RawType) -> Self?
  func toRaw() -> RawType
}

// Workaround for our lack of circular conformance checking. Allow == to be
// defined on _RawOptionSet in order to satisfy the Equatable requirement of
// RawOptionSet without a circularity our type-checker can't yet handle.
protocol _RawOptionSet: RawRepresentable {
  typealias RawType : BitwiseOperations, Equatable
}

// TODO: This is an incomplete implementation of our option sets vision.
protocol RawOptionSet : _RawOptionSet, LogicValue, Equatable
                        /*FIXME: , BitwiseOperations*/ {
  // A non-failable version of RawRepresentable.fromRaw.
  class func fromMask(raw: RawType) -> Self

  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  // The Clang importer synthesizes these for imported NS_OPTIONS.

  /* class func fromRaw(raw: RawType) -> Self? { return fromMask(raw) } */

  /* func getLogicValue() -> Bool { return toRaw() != .allZeros() } */
}

protocol _BuiltinIntegerLiteralConvertible {
  class func _convertFromBuiltinIntegerLiteral(
                value: MaxBuiltinIntegerType) -> Self
}

protocol IntegerLiteralConvertible {
  typealias IntegerLiteralType : _BuiltinIntegerLiteralConvertible
  class func convertFromIntegerLiteral(value: IntegerLiteralType) -> Self
}

protocol _BuiltinFloatLiteralConvertible {
  class func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol FloatLiteralConvertible {
  typealias FloatLiteralType : _BuiltinFloatLiteralConvertible
  class func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}

protocol _BuiltinCharacterLiteralConvertible {
  class func _convertFromBuiltinCharacterLiteral(value: Builtin.Int32) -> Self
}

protocol CharacterLiteralConvertible {
  typealias CharacterLiteralType : _BuiltinCharacterLiteralConvertible
  class func convertFromCharacterLiteral(value: CharacterLiteralType) -> Self
}

protocol _BuiltinExtendedGraphemeClusterLiteralConvertible {
  class func _convertFromBuiltinExtendedGraphemeClusterLiteral(
      start: Builtin.RawPointer,
      byteSize: Builtin.Word,
      isASCII: Builtin.Int1) -> Self
}

protocol ExtendedGraphemeClusterLiteralConvertible {
  typealias ExtendedGraphemeClusterLiteralType : _BuiltinExtendedGraphemeClusterLiteralConvertible
  class func convertFromExtendedGraphemeClusterLiteral(
      value: ExtendedGraphemeClusterLiteralType) -> Self
}

protocol _BuiltinStringLiteralConvertible : _BuiltinExtendedGraphemeClusterLiteralConvertible {
  class func _convertFromBuiltinStringLiteral(start: Builtin.RawPointer,
                                              byteSize: Builtin.Word,
                                              isASCII: Builtin.Int1) -> Self
}

protocol _BuiltinUTF16StringLiteralConvertible : _BuiltinStringLiteralConvertible {
  class func _convertFromBuiltinUTF16StringLiteral(
                start: Builtin.RawPointer,
                numberOfCodeUnits: Builtin.Word) -> Self
}

protocol StringLiteralConvertible : ExtendedGraphemeClusterLiteralConvertible {
  // FIXME: when we have default function implementations in protocols, provide
  // an implementation of convertFromExtendedGraphemeClusterLiteral().

  typealias StringLiteralType : _BuiltinStringLiteralConvertible
  class func convertFromStringLiteral(value: StringLiteralType) -> Self
}

protocol ArrayLiteralConvertible {
  typealias Element
  class func convertFromArrayLiteral(elements: Element...) -> Self
}

protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  class func convertFromDictionaryLiteral(elements: (Key, Value)...) -> Self
}

protocol StringInterpolationConvertible {
  class func convertFromStringInterpolation(strings: Self...) -> Self
}

//===----------------------------------------------------------------------===//
// REPL protocols
//===----------------------------------------------------------------------===//

protocol ReplPrintable {
  func replPrint()
}
