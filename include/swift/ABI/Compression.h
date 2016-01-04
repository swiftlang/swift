//===--- Compression.h - Defines the compression interface ------*- C++ -*-===//
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
#ifndef SWIFT_ABI_COMPRESSION_H
#define SWIFT_ABI_COMPRESSION_H

#include "llvm/ADT/APInt.h"
#include <string>

namespace swift {
namespace Compress {

/// Compress a string using the swift codebook compression.
/// Returns a new compressed string from \p In.
std::string DecodeCBCString(llvm::StringRef In);

/// Decompress a string using the swift codebook compression.
/// Returns a new decompressed string from \p In.
std::string EncodeCBCString(llvm::StringRef In);

/// Character encoding kind:
/// Variable - huffman encoding using variable length characters.
/// Fixed - simple fixed length characters.
enum class EncodingKind { Variable, Fixed };

/// Convert the string \p In into a number using a fixed length or variable
/// length encoding.
llvm::APInt EncodeStringAsNumber(llvm::StringRef In, EncodingKind Kind);

/// Convert the number \p In into a string using a fixed length or variable
/// length encoding.
std::string DecodeStringFromNumber(const llvm::APInt &In, EncodingKind Kind);

/// Compress the string \p In with CBC and variable length encoding.
/// On error return an empty string.
std::string CompressName(llvm::StringRef In);

/// Decompress the string \p, which is compressed using the swift name
/// compression. On error return an empty string.
std::string DecompressName(llvm::StringRef In);

} // namespace Compress
} // namespace swift
#endif /* SWIFT_ABI_COMPRESSION_H */

