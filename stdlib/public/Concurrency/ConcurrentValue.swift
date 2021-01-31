////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2020 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift

/// The ConcurrentValue protocol indicates that value of the given type can
/// be safely used in concurrent code.
public protocol ConcurrentValue { }

extension Array: ConcurrentValue where Element: ConcurrentValue { }
extension ArraySlice: ConcurrentValue where Element: ConcurrentValue { }
extension Bool: ConcurrentValue { }
extension AutoreleasingUnsafeMutablePointer: ConcurrentValue { }
extension Character: ConcurrentValue { }
extension KeyedEncodingContainer: ConcurrentValue { }
extension KeyedDecodingContainer: ConcurrentValue { }
extension CodingUserInfoKey: ConcurrentValue { }
extension EncodingError: ConcurrentValue { }
extension DecodingError: ConcurrentValue { }
extension IndexingIterator: ConcurrentValue { }
extension ContiguousArray: ConcurrentValue { }
extension ClosedRange: ConcurrentValue where Bound: ConcurrentValue { }
extension ClosedRange.Index: ConcurrentValue where Bound: ConcurrentValue { }
extension OpaquePointer: ConcurrentValue { }
extension CVaListPointer: ConcurrentValue { }
extension Dictionary: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension Dictionary.Keys: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension Dictionary.Values: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension Dictionary.Keys.Iterator: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension Dictionary.Values.Iterator: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension Dictionary.Index: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension Dictionary.Iterator: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension EmptyCollection: ConcurrentValue { }
extension EmptyCollection.Iterator: ConcurrentValue { }
extension Hasher: ConcurrentValue { }
extension DefaultIndices: ConcurrentValue where Elements: ConcurrentValue { }
extension KeyValuePairs: ConcurrentValue where Key: ConcurrentValue, Value: ConcurrentValue { }
extension ManagedBufferPointer: ConcurrentValue where Header: ConcurrentValue, Element: ConcurrentValue { }
extension Unicode.Scalar: ConcurrentValue { }
extension Unicode.Scalar.UTF16View: ConcurrentValue { }

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension Unicode.Scalar.UTF8View: ConcurrentValue { }

extension ObjectIdentifier: ConcurrentValue { }
extension SystemRandomNumberGenerator: ConcurrentValue { }
extension Range: ConcurrentValue where Bound: ConcurrentValue { }
extension PartialRangeUpTo: ConcurrentValue where Bound: ConcurrentValue { }
extension PartialRangeThrough: ConcurrentValue where Bound: ConcurrentValue { }
extension PartialRangeFrom: ConcurrentValue where Bound: ConcurrentValue { }
extension PartialRangeFrom.Iterator: ConcurrentValue where Bound: ConcurrentValue { }
extension Repeated: ConcurrentValue where Element: ConcurrentValue { }
extension IteratorSequence: ConcurrentValue where Base: ConcurrentValue { }
extension Set: ConcurrentValue where Element: ConcurrentValue { }
extension Set.Index: ConcurrentValue where Element: ConcurrentValue { }
extension Set.Iterator: ConcurrentValue where Element: ConcurrentValue { }
extension Slice: ConcurrentValue where Base: ConcurrentValue { }
extension StaticString: ConcurrentValue { }
extension StrideToIterator: ConcurrentValue where Element: ConcurrentValue { }
extension StrideTo: ConcurrentValue where Element: ConcurrentValue { }
extension StrideThroughIterator: ConcurrentValue where Element: ConcurrentValue { }
extension StrideThrough: ConcurrentValue where Element: Strideable { }
extension String: ConcurrentValue { }
extension String.Iterator: ConcurrentValue { }
extension String.Index: ConcurrentValue { }
extension DefaultStringInterpolation: ConcurrentValue { }
extension String.UnicodeScalarView: ConcurrentValue { }
extension String.UnicodeScalarView.Iterator: ConcurrentValue { }
extension String.UTF16View: ConcurrentValue { }
extension String.UTF16View.Iterator: ConcurrentValue { }
extension String.UTF8View: ConcurrentValue { }
extension Substring: ConcurrentValue { }
extension Substring.UnicodeScalarView: ConcurrentValue { }
extension Substring.UTF16View: ConcurrentValue { }
extension Substring.UTF8View: ConcurrentValue { }
extension Unicode.Scalar.Properties: ConcurrentValue { }
extension Unicode.CanonicalCombiningClass: ConcurrentValue { }
extension Unmanaged: ConcurrentValue { }
extension UnsafePointer: ConcurrentValue { }
extension UnsafeMutablePointer: ConcurrentValue { }
extension UnsafeRawPointer: ConcurrentValue { }
extension UnsafeMutableRawPointer: ConcurrentValue { }
extension Unicode.UTF8.ForwardParser: ConcurrentValue { }
extension Unicode.UTF8.ReverseParser: ConcurrentValue { }
extension Unicode.UTF16.ForwardParser: ConcurrentValue { }
extension Unicode.UTF16.ReverseParser: ConcurrentValue { }
extension Unicode.UTF32.Parser: ConcurrentValue { }
extension Unicode.ParseResult: ConcurrentValue where T: ConcurrentValue { }
extension Unicode.GeneralCategory: ConcurrentValue { }
extension Unicode.NumericType: ConcurrentValue { }
extension Unicode.UTF8: ConcurrentValue { }
extension Unicode.UTF16: ConcurrentValue { }
extension Unicode.UTF32: ConcurrentValue { }
extension UnicodeDecodingResult: ConcurrentValue { }

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension CollectionDifference: ConcurrentValue where ChangeElement: ConcurrentValue { }
@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension CollectionDifference.Change: ConcurrentValue where ChangeElement: ConcurrentValue { }
@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension CollectionDifference.Index: ConcurrentValue where ChangeElement: ConcurrentValue { }

extension CollectionOfOne: ConcurrentValue where Element: ConcurrentValue { }
extension CollectionOfOne.Iterator: ConcurrentValue where Element: ConcurrentValue { }
extension Mirror: ConcurrentValue { }
extension Mirror.AncestorRepresentation: ConcurrentValue { }
extension Mirror.DisplayStyle: ConcurrentValue { }

// FIXME: Float16 when available
extension Float: ConcurrentValue { }
extension Double: ConcurrentValue { }
// FIXME: Float80 when available

// FIXME: hacks to enumerate integer types, and ".Words", and "SIMDnStorage"
extension UInt8: ConcurrentValue { }
extension Int8: ConcurrentValue { }
extension UInt16: ConcurrentValue { }
extension Int16: ConcurrentValue { }
extension UInt32: ConcurrentValue { }
extension Int32: ConcurrentValue { }
extension UInt64: ConcurrentValue { }
extension Int64: ConcurrentValue { }
extension UInt: ConcurrentValue { }
extension Int: ConcurrentValue { }

extension UnsafeMutableBufferPointer: ConcurrentValue { }
extension UnsafeBufferPointer: ConcurrentValue { }
extension UnsafeBufferPointer.Iterator: ConcurrentValue { }
extension UnsafeMutableRawBufferPointer: ConcurrentValue { }
extension UnsafeRawBufferPointer: ConcurrentValue { }
extension UnsafeRawBufferPointer.Iterator: ConcurrentValue { }

extension SIMDMask: ConcurrentValue where Storage: ConcurrentValue { }
// FIXME: enumerate SIMD types
extension SIMD2: ConcurrentValue where Scalar: ConcurrentValue { }
extension SIMD3: ConcurrentValue where Scalar: ConcurrentValue { }
extension SIMD4: ConcurrentValue where Scalar: ConcurrentValue { }
extension SIMD8: ConcurrentValue where Scalar: ConcurrentValue { }
extension SIMD16: ConcurrentValue where Scalar: ConcurrentValue { }
extension SIMD32: ConcurrentValue where Scalar: ConcurrentValue { }
extension SIMD64: ConcurrentValue where Scalar: ConcurrentValue { }

extension PartialKeyPath: ConcurrentValue where Root: ConcurrentValue { }

extension FloatingPointSign: ConcurrentValue { }
extension FloatingPointClassification: ConcurrentValue { }
extension FloatingPointRoundingRule: ConcurrentValue { }

extension Optional: ConcurrentValue where Wrapped: ConcurrentValue { }
extension Never: ConcurrentValue { }
extension Result: ConcurrentValue where Success: ConcurrentValue, Failure: ConcurrentValue { }

