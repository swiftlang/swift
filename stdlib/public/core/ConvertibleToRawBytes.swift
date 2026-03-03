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
