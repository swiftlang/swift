//===--- shims.h ----------------------------------------------------------===//
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
#ifndef SWIFT_STDLIB_SHIMS_SHIMS_H_
#define SWIFT_STDLIB_SHIMS_SHIMS_H_

//===--- Standard "C" headers ---------------------------------------------===//
#include <stddef.h>        // for size_t
#include <stdint.h>        // for intptr_t

//===--- Forward-declare bits of Darwin that we need in the stdlib --------===//
// Actually #including the headers will pull in our overlay, resulting in a   //
// circular module dependency.

size_t malloc_size(const void *ptr);
size_t strlen(const char *s);
char *strcpy(char *restrict dst, const char *restrict src);
int strcmp(const char *s1, const char *s2);
int putchar(int c);

// This struct is layout-compatible with NSRange.  Using the name
// "NSRange" here could eliminate some horrible unsafeBitCast
// shenanigans in our briging code, but swift's module importer is not
// yet tolerant of the same struct coming in from two different Clang
// modules. <rdar://problem/16294674>
typedef struct {
  intptr_t location;
  intptr_t length;
} _SwiftNSRange;

#ifdef __OBJC2__
typedef struct {
    unsigned long state;
    id __unsafe_unretained *itemsPtr;
    unsigned long *mutationsPtr;
    unsigned long extra[5];
} _SwiftNSFastEnumerationState;
#endif

extern const uint8_t *_swift_stdlib_GraphemeClusterBreakPropertyTrie;

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

extern const struct _swift_stdlib_GraphemeClusterBreakPropertyTrieMetadataTy
_swift_stdlib_GraphemeClusterBreakPropertyTrieMetadata;

extern const uint16_t *
_swift_stdlib_ExtendedGraphemeClusterNoBoundaryRulesMatrix;

//===--- Shims for API availability ---------------------------------------===//

// This struct is layout-compatible with NSOperatingSystemVersion.
typedef struct {
  intptr_t majorVersion;
  intptr_t minorVersion;
  intptr_t patchVersion;
} _SwiftNSOperatingSystemVersion;

_SwiftNSOperatingSystemVersion _swift_stdlib_operatingSystemVersion();

#endif

