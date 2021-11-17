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
typedef enum UErrorCode {} UErrorCode;
typedef enum UCharNameChoice {} UCharNameChoice;
typedef uint16_t UChar;
typedef int32_t UChar32;
typedef int8_t UBool;
typedef __swift_stdlib_UProperty UProperty;

#define U_MAX_VERSION_LENGTH 4
typedef uint8_t UVersionInfo[U_MAX_VERSION_LENGTH];

// Comparison and character property APIs
void u_charAge(UChar32, UVersionInfo);
int32_t u_getIntPropertyValue(UChar32, UProperty);
int32_t u_charName(UChar32, UCharNameChoice, char *, int32_t, UErrorCode *);
}

#else

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdocumentation"

#include <unicode/ustring.h>
#include <unicode/ucol.h>
#include <unicode/ucoleitr.h>
#include <unicode/uiter.h>
#include <unicode/uchar.h>
#include <unicode/ustring.h>
#include <unicode/uvernum.h>
#include <unicode/uversion.h>

#pragma clang diagnostic pop

#endif

namespace {
template <typename T, typename U> T *ptr_cast(U *p) {
  return static_cast<T *>(static_cast<void *>(p));
}
template <typename T, typename U> const T *ptr_cast(const U *p) {
  return static_cast<const T *>(static_cast<const void *>(p));
}
}

void
__swift_stdlib_u_charAge(__swift_stdlib_UChar32 c,
                                __swift_stdlib_UVersionInfo versionInfo) {
  return u_charAge(c, versionInfo);
}

__swift_int32_t
__swift_stdlib_u_getIntPropertyValue(__swift_stdlib_UChar32 c,
                                            __swift_stdlib_UProperty p) {
  return u_getIntPropertyValue(c, static_cast<UProperty>(p));
}

__swift_int32_t __swift_stdlib_u_charName(
    __swift_stdlib_UChar32 code, __swift_stdlib_UCharNameChoice nameChoice,
    char *buffer, __swift_int32_t bufferLength,
    __swift_stdlib_UErrorCode *pErrorCode) {
  return u_charName(code, static_cast<UCharNameChoice>(nameChoice),
                    buffer, bufferLength,
                    ptr_cast<UErrorCode>(pErrorCode));
}

// Force an autolink with ICU
#if defined(__MACH__)
asm(".linker_option \"-licucore\"\n");
#endif // defined(__MACH__)

