//===--- CoreFoundationShims.h - Access to CF for the core stdlib ---------===//
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
//  Using the CoreFoundation module in the core stdlib would create a
//  circular dependency, so instead we import these declarations as
//  part of SwiftShims.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_STDLIB_SHIMS_COREFOUNDATIONSHIMS_H
#define SWIFT_STDLIB_SHIMS_COREFOUNDATIONSHIMS_H

#include "SwiftStdint.h"

#if __LLP64__
typedef unsigned long long _swift_shims_CFTypeID;
typedef unsigned long long _swift_shims_CFOptionFlags;
typedef unsigned long long _swift_shims_CFHashCode;
typedef signed long long _swift_shims_CFIndex;
#else
typedef unsigned long _swift_shims_CFTypeID;
typedef unsigned long _swift_shims_CFOptionFlags;
typedef unsigned long _swift_shims_CFHashCode;
typedef signed long _swift_shims_CFIndex;
#endif

typedef struct {
  _swift_shims_CFIndex location;
  _swift_shims_CFIndex length;
} _swift_shims_CFRange;

typedef id _swift_shims_CFStringRef;
typedef const struct _swift_shims_CFAllocator * _swift_shims_CFAllocatorRef;
typedef __swift_uint32_t _swift_shims_CFStringEncoding;
typedef _swift_shims_CFOptionFlags _swift_shims_CFStringCompareFlags;
typedef _swift_shims_CFIndex _swift_shims_CFComparisonResult;

// Consider creating SwiftMacTypes.h for these
typedef unsigned char _swift_shims_Boolean;
typedef __swift_uint16_t _swift_shims_UniChar;

// Buffer is nullable in case the string is zero-length.
void CFStringGetCharacters(
  _swift_shims_CFStringRef __nonnull theString, _swift_shims_CFRange range, 
  _swift_shims_UniChar * __nullable buffer);

const _swift_shims_UniChar * __nullable CFStringGetCharactersPtr(
  _swift_shims_CFStringRef __nonnull theString);

_swift_shims_CFIndex CFStringGetLength(
  _swift_shims_CFStringRef __nonnull theString);

_swift_shims_CFStringRef __nonnull CFStringCreateWithSubstring(
  _swift_shims_CFAllocatorRef __nullable alloc,
  _swift_shims_CFStringRef __nonnull str, 
  _swift_shims_CFRange range);

_swift_shims_UniChar CFStringGetCharacterAtIndex(
  _swift_shims_CFStringRef __nonnull theString, _swift_shims_CFIndex idx);

_swift_shims_CFStringRef __nonnull CFStringCreateCopy(
  _swift_shims_CFAllocatorRef __nullable alloc,
  _swift_shims_CFStringRef __nonnull theString);

const char * __nullable CFStringGetCStringPtr(
  _swift_shims_CFStringRef __nonnull theString,
  _swift_shims_CFStringEncoding encoding);

_swift_shims_CFComparisonResult CFStringCompare(
  _swift_shims_CFStringRef __nonnull theString1, _swift_shims_CFStringRef __nonnull theString2,
   _swift_shims_CFStringCompareFlags compareOptions);

_swift_shims_Boolean CFStringFindWithOptions(
  _swift_shims_CFStringRef __nonnull theString, 
  _swift_shims_CFStringRef __nonnull stringToFind,
  _swift_shims_CFRange rangeToSearch,
  _swift_shims_CFStringCompareFlags searchOptions,
  _swift_shims_CFRange * __nullable result);

#endif
