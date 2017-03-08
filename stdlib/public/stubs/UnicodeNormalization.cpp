//===--- UnicodeNormalization.cpp - Unicode Normalization Helpers ---------===//
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
// Functions that use ICU to do unicode normalization and collation.
//
//===----------------------------------------------------------------------===//

#if !defined(__APPLE__)
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"

#include <algorithm>
#include <mutex>
#include <assert.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdocumentation"

#include <unicode/ustring.h>
#include <unicode/ucol.h>
#include <unicode/ucoleitr.h>
#include <unicode/uiter.h>

#pragma clang diagnostic pop

#include "../SwiftShims/UnicodeShims.h"

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
  return SWIFT_LAZY_CONSTANT(MakeRootCollator());
}

/// This class caches the collation element results for the ASCII subset of
/// unicode.
class ASCIICollation {
public:
  friend class swift::Lazy<ASCIICollation>;

  static swift::Lazy<ASCIICollation> theTable;
  static const ASCIICollation *getTable() {
    return &theTable.get();
  }

  int32_t CollationTable[128];

  /// Maps an ASCII character to a collation element priority as would be
  /// returned by a call to ucol_next().
  int32_t map(unsigned char c) const {
    return CollationTable[c];
  }

private:
  /// Construct the ASCII collation table.
  ASCIICollation() {
    const UCollator *Collator = GetRootCollator();
    for (unsigned char c = 0; c < 128; ++c) {
      UErrorCode ErrorCode = U_ZERO_ERROR;
      intptr_t NumCollationElts = 0;
#if defined(__CYGWIN__) || defined(_MSC_VER)
      UChar Buffer[1];
#else
      uint16_t Buffer[1];
#endif
      Buffer[0] = c;

      UCollationElements *CollationIterator =
          ucol_openElements(Collator, Buffer, 1, &ErrorCode);

      while (U_SUCCESS(ErrorCode)) {
        intptr_t Elem = ucol_next(CollationIterator, &ErrorCode);
        if (Elem != UCOL_NULLORDER) {
          CollationTable[c] = Elem;
          ++NumCollationElts;
        } else {
          break;
        }
      }

      ucol_closeElements(CollationIterator);
      if (U_FAILURE(ErrorCode) || NumCollationElts != 1) {
        swift::crash("Error setting up the ASCII collation table");
      }
    }
  }

  ASCIICollation &operator=(const ASCIICollation &) = delete;
  ASCIICollation(const ASCIICollation &) = delete;
};

/// Compares the strings via the Unicode Collation Algorithm on the root locale.
/// Results are the usual string comparison results:
///  <0 the left string is less than the right string.
/// ==0 the strings are equal according to their collation.
///  >0 the left string is greater than the right string.
int32_t
swift::_swift_stdlib_unicode_compare_utf16_utf16(const uint16_t *LeftString,
                                                 int32_t LeftLength,
                                                 const uint16_t *RightString,
                                                 int32_t RightLength) {
#if defined(__CYGWIN__) || defined(_MSC_VER)
  // ICU UChar type is platform dependent. In Cygwin, it is defined
  // as wchar_t which size is 2. It seems that the underlying binary
  // representation is same with swift utf16 representation.
  return ucol_strcoll(GetRootCollator(),
    reinterpret_cast<const UChar *>(LeftString), LeftLength,
    reinterpret_cast<const UChar *>(RightString), RightLength);
#else
  return ucol_strcoll(GetRootCollator(),
    LeftString, LeftLength,
    RightString, RightLength);
#endif
}

/// Compares the strings via the Unicode Collation Algorithm on the root locale.
/// Results are the usual string comparison results:
///  <0 the left string is less than the right string.
/// ==0 the strings are equal according to their collation.
///  >0 the left string is greater than the right string.
int32_t
swift::_swift_stdlib_unicode_compare_utf8_utf16(const unsigned char *LeftString,
                                                int32_t LeftLength,
                                                const uint16_t *RightString,
                                                int32_t RightLength) {
  UCharIterator LeftIterator;
  UCharIterator RightIterator;
  UErrorCode ErrorCode = U_ZERO_ERROR;

  uiter_setUTF8(&LeftIterator, reinterpret_cast<const char *>(LeftString), LeftLength);
#if defined(__CYGWIN__) || defined(_MSC_VER)
  uiter_setString(&RightIterator, reinterpret_cast<const UChar *>(RightString),
                  RightLength);
#else
  uiter_setString(&RightIterator, RightString, RightLength);
#endif

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
int32_t
swift::_swift_stdlib_unicode_compare_utf8_utf8(const unsigned char *LeftString,
                                               int32_t LeftLength,
                                               const unsigned char *RightString,
                                               int32_t RightLength) {
  UCharIterator LeftIterator;
  UCharIterator RightIterator;
  UErrorCode ErrorCode = U_ZERO_ERROR;

  uiter_setUTF8(&LeftIterator, reinterpret_cast<const char *>(LeftString), LeftLength);
  uiter_setUTF8(&RightIterator, reinterpret_cast<const char *>(RightString), RightLength);

  uint32_t Diff = ucol_strcollIter(GetRootCollator(),
    &LeftIterator, &RightIterator, &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("ucol_strcollIter: Unexpected error doing utf8<->utf8 string comparison.");
  }
  return Diff;
}

/// Used by _swift_stdlib_unicode_find_longest_contraction below.
static int32_t CachedLongestContraction = -1;

/// Finds and returns the longest contraction defined by the root collator.
/// Results is the length of the longest contraction (in UChars, i.e. UTF16 code units).
/// The result is cached in the global static CachedLongestContraction.
int32_t
swift::_swift_stdlib_unicode_find_longest_contraction(void) {
  // In order to play nice with other threads that enter this function
  // on SMP systems we copy CachedLongestContraction to a local and use
  // that during for calculations, only updating it once we're ready
  // to return to the caller. We don't need any sort of synchronization
  // because we expect this function to be idempotent.
  int32_t LocalLongestContraction = CachedLongestContraction;
  if (LocalLongestContraction >= 0)
    return LocalLongestContraction;
  USet *Contractions = uset_openEmpty();
  UErrorCode ErrorCode = U_ZERO_ERROR;
  if (!Contractions) {
    swift::crash("uset_openEmpty: Unable to create a new set.");
  }
  std::unique_ptr<USet, decltype(&uset_close)> ContractionsPtr(Contractions, uset_close);
  ucol_getContractionsAndExpansions(GetRootCollator(), Contractions, nullptr, FALSE, &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("ucol_getContractionsAndExpansions: Unable to get root collator's contractions.");
  }
  int32_t NumContractions = uset_getItemCount(Contractions);
  UChar32 Start, End;
  for (int32_t i = 0; i < NumContractions; ++i) {
    int32_t ItemLength = uset_getItem(Contractions, i, &Start, &End, nullptr, 0,
                                      &ErrorCode);
    assert(ItemLength > 0 && "Expecting the set of contractions to only contain strings, not ranges");
    if (ErrorCode == U_BUFFER_OVERFLOW_ERROR)
      ErrorCode = U_ZERO_ERROR;
    if (U_FAILURE(ErrorCode)) {
      swift::crash("uset_getItem: Unable to get item from set.");
    }
    if (ItemLength > LocalLongestContraction)
      LocalLongestContraction = ItemLength;
  }
  CachedLongestContraction = LocalLongestContraction;
  return LocalLongestContraction;
}

void *swift::_swift_stdlib_unicodeCollationIterator_create(
    const __swift_uint16_t *Str, __swift_uint32_t Length) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
#if defined(__CYGWIN__) || defined(_MSC_VER)
  UCollationElements *CollationIterator = ucol_openElements(
    GetRootCollator(), reinterpret_cast<const UChar *>(Str), Length,
    &ErrorCode);
#else
  UCollationElements *CollationIterator = ucol_openElements(
    GetRootCollator(), Str, Length, &ErrorCode);
#endif
  if (U_FAILURE(ErrorCode)) {
    swift::crash("_swift_stdlib_unicodeCollationIterator_create: ucol_openElements() failed.");
  }
  return CollationIterator;
}

__swift_int32_t swift::_swift_stdlib_unicodeCollationIterator_next(
    void *CollationIterator, bool *HitEnd) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
  auto Result = ucol_next(
      static_cast<UCollationElements *>(CollationIterator), &ErrorCode);
  if (U_FAILURE(ErrorCode)) {
    swift::crash("_swift_stdlib_unicodeCollationIterator_next: ucol_next() failed.");
  }
  *HitEnd = (Result == UCOL_NULLORDER);
  return Result;
}

void swift::_swift_stdlib_unicodeCollationIterator_delete(
    void *CollationIterator) {
  ucol_closeElements(static_cast<UCollationElements *>(CollationIterator));
}

const __swift_int32_t *swift::_swift_stdlib_unicode_getASCIICollationTable() {
  return ASCIICollation::getTable()->CollationTable;
}

/// Convert the unicode string to uppercase. This function will return the
/// required buffer length as a result. If this length does not match the
/// 'DestinationCapacity' this function must be called again with a buffer of
/// the required length to get an uppercase version of the string.
int32_t
swift::_swift_stdlib_unicode_strToUpper(uint16_t *Destination,
                                        int32_t DestinationCapacity,
                                        const uint16_t *Source,
                                        int32_t SourceLength) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
#if defined(__CYGWIN__) || defined(_MSC_VER)
  uint32_t OutputLength = u_strToUpper(reinterpret_cast<UChar *>(Destination),
                                       DestinationCapacity,
                                       reinterpret_cast<const UChar *>(Source),
                                       SourceLength,
                                       "", &ErrorCode);
#else
  uint32_t OutputLength = u_strToUpper(Destination, DestinationCapacity,
                                       Source, SourceLength,
                                       "", &ErrorCode);
#endif
  if (U_FAILURE(ErrorCode) && ErrorCode != U_BUFFER_OVERFLOW_ERROR) {
    swift::crash("u_strToUpper: Unexpected error uppercasing unicode string.");
  }
  return OutputLength;
}

/// Convert the unicode string to lowercase. This function will return the
/// required buffer length as a result. If this length does not match the
/// 'DestinationCapacity' this function must be called again with a buffer of
/// the required length to get a lowercase version of the string.
int32_t
swift::_swift_stdlib_unicode_strToLower(uint16_t *Destination,
                                        int32_t DestinationCapacity,
                                        const uint16_t *Source,
                                        int32_t SourceLength) {
  UErrorCode ErrorCode = U_ZERO_ERROR;
#if defined(__CYGWIN__) || defined(_MSC_VER)
  uint32_t OutputLength = u_strToLower(reinterpret_cast<UChar *>(Destination),
                                       DestinationCapacity,
                                       reinterpret_cast<const UChar *>(Source),
                                       SourceLength,
                                       "", &ErrorCode);
#else
  uint32_t OutputLength = u_strToLower(Destination, DestinationCapacity,
                                       Source, SourceLength,
                                       "", &ErrorCode);
#endif
  if (U_FAILURE(ErrorCode) && ErrorCode != U_BUFFER_OVERFLOW_ERROR) {
    swift::crash("u_strToLower: Unexpected error lowercasing unicode string.");
  }
  return OutputLength;
}

swift::Lazy<ASCIICollation> ASCIICollation::theTable;
#endif

