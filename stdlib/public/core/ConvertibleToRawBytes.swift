//===--- ConvertibleToRawBytes.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A protocol for types whose memory can safely be read as individual raw bytes.
///
/// A type can conform to ConvertibleToRawBytes if its memory representation
/// includes no padding. The sum of the size of its stored properties must be
/// equal to its stride.
///
/// A type that conforms to ConvertibleToRawBytes must have:
/// * one or more stored properties,
/// * all of its stored properties have a type which conforms to
///   `ConvertibleToRawBytes`,
/// * its stored properties are stored contiguously in memory, with no padding,
/// * none of its values disregards a subset of its bytes, making most enums
///   ineligible.
@_marker public protocol ConvertibleToRawBytes: Copyable {}

/// A protocol for types whose memory can safely be populated from raw bytes,
/// resulting in a valid instance.
///
/// A type can conform to ConvertibleFromRawBytes if every bit pattern for
/// every byte of its stored properties is valid. Note that this allows
/// conformances for types with internal or trailing padding.
/// A conformer to ConvertibleFromRawBytes must not have semantic constraints
/// on the values of its stored properties.
/// All its stored properties must themselves conform to ConvertibleFromRawBytes.
@_marker public protocol ConvertibleFromRawBytes: BitwiseCopyable {}

/// A protocol for types whose memory can safely be written as or read from
/// raw bytes.
public typealias FullyInhabited = ConvertibleToRawBytes & ConvertibleFromRawBytes

extension UInt8:   ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension Int8:    ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension UInt16:  ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension Int16:   ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension UInt32:  ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension Int32:   ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension UInt64:  ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension Int64:   ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension UInt:    ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension Int:     ConvertibleToRawBytes, ConvertibleFromRawBytes {}

@available(SwiftStdlib 6.0, *)
extension UInt128: ConvertibleToRawBytes, ConvertibleFromRawBytes {}
@available(SwiftStdlib 6.0, *)
extension Int128:  ConvertibleToRawBytes, ConvertibleFromRawBytes {}

//extension Float16: ConvertibleToRawBytes, ConvertibleFromRawBytes {}
extension Float32: ConvertibleToRawBytes, ConvertibleFromRawBytes {} // `Float`
extension Float64: ConvertibleToRawBytes, ConvertibleFromRawBytes {} // `Double`

@available(StdlibDeploymentTarget 5.7, *)
extension Duration: ConvertibleToRawBytes, ConvertibleFromRawBytes {}

@available(SwiftStdlib 6.2, *)
extension InlineArray: ConvertibleToRawBytes where Element: ConvertibleToRawBytes {}
@available(SwiftStdlib 6.2, *)
extension InlineArray: ConvertibleFromRawBytes where Element: ConvertibleFromRawBytes {}

extension CollectionOfOne: ConvertibleToRawBytes where Element: ConvertibleToRawBytes {}
extension CollectionOfOne: ConvertibleFromRawBytes where Element: ConvertibleFromRawBytes {}

extension ClosedRange: ConvertibleToRawBytes where Bound: ConvertibleToRawBytes {}
extension Range: ConvertibleToRawBytes where Bound: ConvertibleToRawBytes {}
extension IndexingIterator: ConvertibleToRawBytes
  where Elements: ConvertibleToRawBytes, Elements.Index: ConvertibleToRawBytes {}

extension PartialRangeFrom: ConvertibleToRawBytes where Bound: ConvertibleToRawBytes {}
extension PartialRangeFrom.Iterator: ConvertibleToRawBytes
  where Bound: ConvertibleToRawBytes {}
extension PartialRangeThrough: ConvertibleToRawBytes where Bound: ConvertibleToRawBytes {}
extension PartialRangeUpTo: ConvertibleToRawBytes where Bound: ConvertibleToRawBytes {}

extension Bool: ConvertibleToRawBytes {}
extension ObjectIdentifier: ConvertibleToRawBytes {}

extension UnsafePointer: ConvertibleToRawBytes {}
extension UnsafeMutablePointer: ConvertibleToRawBytes {}
extension UnsafeRawPointer: ConvertibleToRawBytes {}
extension UnsafeMutableRawPointer: ConvertibleToRawBytes {}
extension OpaquePointer: ConvertibleToRawBytes {}

extension UnsafeBufferPointer: ConvertibleToRawBytes {}
extension UnsafeMutableBufferPointer: ConvertibleToRawBytes {}
extension UnsafeRawBufferPointer: ConvertibleToRawBytes {}
extension UnsafeMutableRawBufferPointer: ConvertibleToRawBytes {}
