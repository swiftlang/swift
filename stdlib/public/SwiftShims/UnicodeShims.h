//===--- UnicodeShims.h - Access to Unicode data for the core stdlib ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "SwiftStdbool.h"
#include "Visibility.h"

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_uint8_t *_swift_stdlib_GraphemeClusterBreakPropertyTrie;

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
const struct _swift_stdlib_GraphemeClusterBreakPropertyTrieMetadataTy
_swift_stdlib_GraphemeClusterBreakPropertyTrieMetadata;

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_uint16_t *
_swift_stdlib_ExtendedGraphemeClusterNoBoundaryRulesMatrix;

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_READONLY __swift_int32_t
_swift_stdlib_unicode_compare_utf16_utf16(const __swift_uint16_t *Left,
                                          __swift_int32_t LeftLength,
                                          const __swift_uint16_t *Right,
                                          __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_READONLY __swift_int32_t
_swift_stdlib_unicode_compare_utf8_utf16(const unsigned char *Left,
                                         __swift_int32_t LeftLength,
                                         const __swift_uint16_t *Right,
                                         __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_READONLY __swift_int32_t
_swift_stdlib_unicode_compare_utf8_utf8(const unsigned char *Left,
                                        __swift_int32_t LeftLength,
                                        const unsigned char *Right,
                                        __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
__attribute__((__pure__)) __swift_int32_t
_swift_stdlib_unicode_find_longest_contraction(void);

SWIFT_RUNTIME_STDLIB_INTERFACE
void *_swift_stdlib_unicodeCollationIterator_create(
    const __swift_uint16_t *Str,
    __swift_uint32_t Length);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t _swift_stdlib_unicodeCollationIterator_next(
    void *CollationIterator, __swift_bool *HitEnd);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_stdlib_unicodeCollationIterator_delete(
    void *CollationIterator);

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_int32_t *_swift_stdlib_unicode_getASCIICollationTable();

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

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif
