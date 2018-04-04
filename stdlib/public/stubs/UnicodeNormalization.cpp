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

#include "../SwiftShims/UnicodeShims.h"

#include <stdint.h>

#if defined(__APPLE__)

// Declare a few external functions to avoid a dependency on ICU headers.
extern "C" {

// Types
typedef struct UBreakIterator UBreakIterator;
typedef struct UBreakIterator UNormalizer2;
typedef enum UBreakIteratorType {} UBreakIteratorType;
typedef enum UErrorCode {} UErrorCode;
typedef uint16_t UChar;
typedef int32_t UChar32;
typedef int8_t UBool;
typedef swift::__swift_stdlib_UProperty UProperty;

// Grapheme breaking APIs
void ubrk_close(UBreakIterator *);
UBreakIterator *ubrk_open(UBreakIteratorType, const char *, const UChar *,
                          int32_t, UErrorCode *);
int32_t ubrk_preceding(UBreakIterator *, int32_t);
int32_t ubrk_following(UBreakIterator *, int32_t);
void ubrk_setText(UBreakIterator *, const UChar *, int32_t, UErrorCode *);

// Comparison, normalization, and character property APIs
int32_t unorm2_spanQuickCheckYes(const UNormalizer2 *, const UChar *, int32_t,
                                 UErrorCode *);
int32_t unorm2_normalize(const UNormalizer2 *, const UChar *, int32_t, UChar *,
                         int32_t, UErrorCode *);
const UNormalizer2 *unorm2_getNFCInstance(UErrorCode *);
UBool unorm2_hasBoundaryBefore(const UNormalizer2 *norm2, UChar32 c);
UBool u_hasBinaryProperty(UChar32, UProperty);
UBool u_isdefined(UChar32);
}

#else

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdocumentation"

#include <unicode/ustring.h>
#include <unicode/ucol.h>
#include <unicode/ucoleitr.h>
#include <unicode/uiter.h>
#include <unicode/ubrk.h>
#include <unicode/uchar.h>
#include <unicode/uvernum.h>

#pragma clang diagnostic pop

#endif

#if !defined(__APPLE__)
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"

#include <algorithm>
#include <mutex>
#include <assert.h>

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
  uint32_t OutputLength = u_strToUpper(reinterpret_cast<UChar *>(Destination),
                                       DestinationCapacity,
                                       reinterpret_cast<const UChar *>(Source),
                                       SourceLength,
                                       "", &ErrorCode);
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
  uint32_t OutputLength = u_strToLower(reinterpret_cast<UChar *>(Destination),
                                       DestinationCapacity,
                                       reinterpret_cast<const UChar *>(Source),
                                       SourceLength,
                                       "", &ErrorCode);
  if (U_FAILURE(ErrorCode) && ErrorCode != U_BUFFER_OVERFLOW_ERROR) {
    swift::crash("u_strToLower: Unexpected error lowercasing unicode string.");
  }
  return OutputLength;
}
#endif

namespace {
template <typename T, typename U> T *ptr_cast(U *p) {
  return static_cast<T *>(static_cast<void *>(p));
}
template <typename T, typename U> const T *ptr_cast(const U *p) {
  return static_cast<const T *>(static_cast<const void *>(p));
}
}

void swift::__swift_stdlib_ubrk_close(
    swift::__swift_stdlib_UBreakIterator *bi) {
  ubrk_close(ptr_cast<UBreakIterator>(bi));
}

swift::__swift_stdlib_UBreakIterator *swift::__swift_stdlib_ubrk_open(
    swift::__swift_stdlib_UBreakIteratorType type, const char *locale,
    const __swift_stdlib_UChar *text, int32_t textLength,
    __swift_stdlib_UErrorCode *status) {
  return ptr_cast<swift::__swift_stdlib_UBreakIterator>(
      ubrk_open(static_cast<UBreakIteratorType>(type), locale,
                reinterpret_cast<const UChar *>(text), textLength,
                ptr_cast<UErrorCode>(status)));
}

int32_t
swift::__swift_stdlib_ubrk_preceding(swift::__swift_stdlib_UBreakIterator *bi,
                                     int32_t offset) {
  return ubrk_preceding(ptr_cast<UBreakIterator>(bi), offset);
}

int32_t
swift::__swift_stdlib_ubrk_following(swift::__swift_stdlib_UBreakIterator *bi,
                                     int32_t offset) {
  return ubrk_following(ptr_cast<UBreakIterator>(bi), offset);
}

void swift::__swift_stdlib_ubrk_setText(
    swift::__swift_stdlib_UBreakIterator *bi, const __swift_stdlib_UChar *text,
    __swift_int32_t textLength, __swift_stdlib_UErrorCode *status) {
  return ubrk_setText(ptr_cast<UBreakIterator>(bi), ptr_cast<UChar>(text),
                      textLength, ptr_cast<UErrorCode>(status));
}

swift::__swift_stdlib_UBool swift::__swift_stdlib_unorm2_hasBoundaryBefore(
    const __swift_stdlib_UNormalizer2 *ptr, __swift_stdlib_UChar32 char32) {
  return unorm2_hasBoundaryBefore(ptr_cast<UNormalizer2>(ptr), char32);
}
const swift::__swift_stdlib_UNormalizer2 *
swift::__swift_stdlib_unorm2_getNFCInstance(__swift_stdlib_UErrorCode *err) {
  return ptr_cast<__swift_stdlib_UNormalizer2>(
      unorm2_getNFCInstance(ptr_cast<UErrorCode>(err)));
}

int32_t swift::__swift_stdlib_unorm2_normalize(
    const __swift_stdlib_UNormalizer2 *norm, const __swift_stdlib_UChar *src,
    __swift_int32_t len, __swift_stdlib_UChar *dst, __swift_int32_t capacity,
    __swift_stdlib_UErrorCode *err) {
  // TODO remove this compatibility when we require ICU >= 59 on Linux
#if defined(__APPLE__) || U_ICU_VERSION_MAJOR_NUM >= 59
  return unorm2_normalize(ptr_cast<UNormalizer2>(norm), src, len, dst, capacity,
                          ptr_cast<UErrorCode>(err));
#else
  return unorm2_normalize(ptr_cast<UNormalizer2>(norm),
                          reinterpret_cast<const UChar *>(src), len,
                          reinterpret_cast<UChar *>(dst), capacity,
                          ptr_cast<UErrorCode>(err));
#endif
}

__swift_int32_t swift::__swift_stdlib_unorm2_spanQuickCheckYes(
    const __swift_stdlib_UNormalizer2 *norm, const __swift_stdlib_UChar *ptr,
    __swift_int32_t len, __swift_stdlib_UErrorCode *err) {
  return unorm2_spanQuickCheckYes(ptr_cast<UNormalizer2>(norm),
                                  ptr_cast<UChar>(ptr), len,
                                  ptr_cast<UErrorCode>(err));
}

swift::__swift_stdlib_UBool
swift::__swift_stdlib_u_hasBinaryProperty(__swift_stdlib_UChar32 c,
                                          __swift_stdlib_UProperty p) {
  return u_hasBinaryProperty(c, static_cast<UProperty>(p));
}

swift::__swift_stdlib_UBool
swift::__swift_stdlib_u_isdefined(UChar32 c) {
  return u_isdefined(c);
}


// Force an autolink with ICU
#if defined(__MACH__)
asm(".linker_option \"-licucore\"\n");
#endif // defined(__MACH__)

