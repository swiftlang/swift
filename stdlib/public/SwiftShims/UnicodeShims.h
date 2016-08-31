//===--- UnicodeShims.h - Access to Unicode data for the core stdlib ------===//
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
//
//  Data structures required for Unicode support in Swift that are
//  statically initialized in its runtime's C++ source.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_STDLIB_SHIMS_UNICODESHIMS_H_
#define SWIFT_STDLIB_SHIMS_UNICODESHIMS_H_

#include "SwiftStdint.h"
#include "Visibility.h"

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

SWIFT_RUNTIME_STDLIB_INTERFACE
extern const __swift_uint8_t *_swift_stdlib_GraphemeClusterBreakPropertyTrie;

struct _swift_stdlib_GraphemeClusterBreakPropertyTrieMetadataTy {
  unsigned BMPFirstLevelIndexBits;
  unsigned BMPDataOffsetBits;
  unsigned SuppFirstLevelIndexBits;
  unsigned SuppSecondLevelIndexBits;
  unsigned SuppDataOffsetBits;

  unsigned BMPLookupBytesPerEntry;
  unsigned BMPDataBytesPerEntry;
  unsigned SuppLookup1BytesPerEntry;
  unsigned SuppLookup2BytesPerEntry;
  unsigned SuppDataBytesPerEntry;

  unsigned TrieSize;

  unsigned BMPLookupBytesOffset;
  unsigned BMPDataBytesOffset;
  unsigned SuppLookup1BytesOffset;
  unsigned SuppLookup2BytesOffset;
  unsigned SuppDataBytesOffset;
};

SWIFT_RUNTIME_STDLIB_INTERFACE
extern const struct _swift_stdlib_GraphemeClusterBreakPropertyTrieMetadataTy
_swift_stdlib_GraphemeClusterBreakPropertyTrieMetadata;

SWIFT_RUNTIME_STDLIB_INTERFACE
extern const __swift_uint16_t *
_swift_stdlib_ExtendedGraphemeClusterNoBoundaryRulesMatrix;

SWIFT_RUNTIME_STDLIB_INTERFACE
__attribute__((__pure__)) __swift_int32_t
_swift_stdlib_unicode_compare_utf16_utf16(const __swift_uint16_t *Left,
                                          __swift_int32_t LeftLength,
                                          const __swift_uint16_t *Right,
                                          __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
__attribute__((__pure__)) __swift_int32_t
_swift_stdlib_unicode_compare_utf8_utf16(const unsigned char *Left,
                                         __swift_int32_t LeftLength,
                                         const __swift_uint16_t *Right,
                                         __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
__attribute__((__pure__)) __swift_int32_t
_swift_stdlib_unicode_compare_utf8_utf8(const unsigned char *Left,
                                        __swift_int32_t LeftLength,
                                        const unsigned char *Right,
                                        __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
__attribute__((__pure__)) __swift_intptr_t
_swift_stdlib_unicode_hash(const __swift_uint16_t *Str, __swift_int32_t Length);

SWIFT_RUNTIME_STDLIB_INTERFACE
__attribute__((__pure__)) __swift_intptr_t
_swift_stdlib_unicode_hash_ascii(const unsigned char *Str,
                                 __swift_int32_t Length);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t _swift_stdlib_unicode_strToUpper(
  __swift_uint16_t *Destination, __swift_int32_t DestinationCapacity,
  const __swift_uint16_t *Source, __swift_int32_t SourceLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t _swift_stdlib_unicode_strToLower(
  __swift_uint16_t *Destination, __swift_int32_t DestinationCapacity,
  const __swift_uint16_t *Source, __swift_int32_t SourceLength);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif
