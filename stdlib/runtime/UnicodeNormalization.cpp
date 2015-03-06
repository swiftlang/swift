//===--- UnicodeNormalization.cpp - Unicode Normalization Helpers ---------===//
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
//
// Functions that use ICU to do unicode normalization and collation.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#include "Debug.h"

#include <algorithm>
#include <mutex>
#include <assert.h>

#include <unicode/ustring.h>
#include <unicode/ucol.h>
#include <unicode/ucoleitr.h>
#include <unicode/uiter.h>

static const UCollator *MakeRootCollator() {
  UErrorCode ErrorCode = U_ZERO_ERROR;
  UCollator *root = ucol_open("", &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("ucol_open: Failure setting up default collation.");
  }
  ucol_setAttribute(root, UCOL_NORMALIZATION_MODE, UCOL_ON, &ErrorCode);
  ucol_setAttribute(root, UCOL_STRENGTH, UCOL_TERTIARY, &ErrorCode);
  ucol_setAttribute(root, UCOL_NUMERIC_COLLATION, UCOL_OFF, &ErrorCode);
  ucol_setAttribute(root, UCOL_CASE_LEVEL, UCOL_OFF, &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("ucol_setAttribute: Failure setting up default collation.");
  }
  return root;
}

// According to this thread in the ICU mailing list, it should be safe
// to assume the UCollator object is thread safe so long as you're only
// passing it to functions that take a const pointer to it. So, we make it
// const here to make sure we don't misuse it.
// http://sourceforge.net/p/icu/mailman/message/27427062/
static const UCollator *GetRootCollator() {
  static const UCollator *RootCollator = MakeRootCollator();
  return RootCollator;
}

/// Compares the strings via the Unicode Collation Algorithm on the root locale.
/// Results are the usual string comparison results:
///  <0 the left string is less than the right string.
/// ==0 the strings are equal according to their collation.
///  >0 the left string is greater than the right string.
extern "C"
int32_t _swift_stdlib_unicode_compare_utf16_utf16(const uint16_t *LeftString,
                                                  int32_t LeftLength,
                                                  const uint16_t *RightString,
                                                  int32_t RightLength) {
  return ucol_strcoll(GetRootCollator(),
    LeftString, LeftLength,
    RightString, RightLength);
}

/// Compares the strings via the Unicode Collation Algorithm on the root locale.
/// Results are the usual string comparison results:
///  <0 the left string is less than the right string.
/// ==0 the strings are equal according to their collation.
///  >0 the left string is greater than the right string.
extern "C"
int32_t _swift_stdlib_unicode_compare_utf8_utf16(const char *LeftString,
                                                 int32_t LeftLength,
                                                 const uint16_t *RightString,
                                                 int32_t RightLength) {
  UCharIterator LeftIterator;
  UCharIterator RightIterator;
  UErrorCode ErrorCode = U_ZERO_ERROR;

  uiter_setUTF8(&LeftIterator, LeftString, LeftLength);
  uiter_setString(&RightIterator, RightString, RightLength);

  uint32_t Diff = ucol_strcollIter(GetRootCollator(),
    &LeftIterator, &RightIterator, &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("ucol_strcollIter: Unexpected error doing utf8<->utf16 string comparison.");
  }
  return Diff;
}

/// Compares the strings via the Unicode Collation Algorithm on the root locale.
/// Results are the usual string comparison results:
///  <0 the left string is less than the right string.
/// ==0 the strings are equal according to their collation.
///  >0 the left string is greater than the right string.
extern "C"
int32_t _swift_stdlib_unicode_compare_utf8_utf8(const char *LeftString,
                                                int32_t LeftLength,
                                                const char *RightString,
                                                int32_t RightLength) {
  UCharIterator LeftIterator;
  UCharIterator RightIterator;
  UErrorCode ErrorCode = U_ZERO_ERROR;

  uiter_setUTF8(&LeftIterator, LeftString, LeftLength);
  uiter_setUTF8(&RightIterator, RightString, RightLength);

  uint32_t Diff = ucol_strcollIter(GetRootCollator(),
    &LeftIterator, &RightIterator, &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("ucol_strcollIter: Unexpected error doing utf8<->utf8 string comparison.");
  }
  return Diff;
}

// These functions use murmurhash2 in its 32 and 64bit forms, which are
// differentiated by the constants defined below. This seems like a good choice
// for now because it operates efficiently in blocks rather than bytes, and 
// the data returned from the collation iterator comes in 4byte chunks.
#if __arm__ || __i386__
#define HASH_SEED 0x88ddcc21
#define HASH_M 0x5bd1e995
#define HASH_R 24
#else
#define HASH_SEED 0x429b126688ddcc21
#define HASH_M 0xc6a4a7935bd1e995
#define HASH_R 47
#endif

static intptr_t hashChunk(const UCollator *Collator, intptr_t HashState,
                          const uint16_t *Str, uint32_t Length,
                          UErrorCode *ErrorCode) {
  UCollationElements *CollationIterator = ucol_openElements(
    Collator, Str, Length, ErrorCode);
  while (U_SUCCESS(*ErrorCode)) {
    intptr_t Elem = ucol_next(CollationIterator, ErrorCode);
    if (Elem != UCOL_NULLORDER) {
      Elem *= HASH_M;
      Elem ^= Elem >> HASH_R;
      Elem *= HASH_M;

      HashState *= HASH_M;
      HashState ^= Elem;
    } else {
      break;
    }
  }
  ucol_closeElements(CollationIterator);
  return HashState;
}
static intptr_t hashFinish(intptr_t HashState) {
  HashState ^= HashState >> HASH_R;
  HashState *= HASH_M;
  HashState ^= HashState >> HASH_R;
  return HashState;
}

extern "C"
intptr_t _swift_stdlib_unicode_hash(const uint16_t *Str, int32_t Length) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
  intptr_t HashState = HASH_SEED ^ (Length * HASH_M);
  HashState = hashChunk(GetRootCollator(), HashState, Str, Length, &ErrorCode);

  if (U_FAILURE(ErrorCode)) {
    swift::crash("hashChunk: Unexpected error hashing unicode string.");
  }
  return hashFinish(HashState);
}

#define ASCII_HASH_BUFFER_SIZE 16
extern "C"
intptr_t _swift_stdlib_unicode_hash_ascii(const char *Str, int32_t Length) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
  const UCollator *Collator = GetRootCollator();
  intptr_t HashState = HASH_SEED ^ (Length * HASH_M);
  uint16_t HashBuffer[ASCII_HASH_BUFFER_SIZE];

  int32_t Pos = 0;
  while (Pos < Length) {
    // Copy into the buffer up to ASCII_HASH_BUFFER_SIZE.
    // Note that we assume that chunking the ASCII string up like this
    // does not cause the collation elements to be wrong because there are
    // no contractions in the 7 bit ascii set in the root locale.
    int32_t BufferPos = 0;
    while (Pos < Length && BufferPos < ASCII_HASH_BUFFER_SIZE) {
      assert((Str[Pos] & 0x80) == 0); // UTF-8 breaks the above assumptions.
      HashBuffer[BufferPos++] = Str[Pos++];
    }
    // Do hash with what's in the buffer now, if anything.
    if (BufferPos > 0) {
      HashState = hashChunk(Collator, HashState,
        HashBuffer, BufferPos,
        &ErrorCode);
    }

    if (U_FAILURE(ErrorCode)) {
      swift::crash("hashChunk: Unexpected error hashing ascii string.");
    }
  }
  return hashFinish(HashState);
}

extern "C"
int32_t _swift_stdlib_unicode_strToUpper(uint16_t *Destination,
                                         int32_t DestinationCapacity,
                                         const uint16_t *Source,
                                         int32_t SourceLength) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
  uint32_t OutputLength = u_strToUpper(Destination, DestinationCapacity,
                                       Source, SourceLength,
                                       "", &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("u_strToUpper: Unexpected error uppercasing unicode string.");
  }
  return OutputLength;
}

extern "C"
int32_t _swift_stdlib_unicode_strToLower(uint16_t *Destination,
                                         int32_t DestinationCapacity,
                                         const uint16_t *Source,
                                         int32_t SourceLength) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
  uint32_t OutputLength = u_strToLower(Destination, DestinationCapacity,
                                       Source, SourceLength,
                                       "", &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("u_strToLower: Unexpected error lowercasing unicode string.");
  }
  return OutputLength;
}
